{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Data.Gettext
  ( -- * Data structures
   GmoFile (..),
   Catalog,
   -- * Loading and using translations
   loadCatalog,
   lookup,
   gettext, ngettext, ngettext',
   assocs,
   -- * Utilities for plural forms
   getHeaders,
   getPluralDefinition,
   choosePluralForm,
   -- * Utilities for custom parsers implementation
   parseGmo,
   unpackGmoFile
  ) where

import Prelude hiding (lookup)
import Control.Monad
import Data.Either
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Trie as Trie
import Data.Word
import Text.Printf

import Data.Gettext.Plural
import Data.Gettext.Parsers

-- import Debug.Trace

-- | This structure describes the binary structure of Gettext @.mo/.gmo@ file.
data GmoFile = GmoFile {
    fMagic :: Word32                       -- ^ Magic number (must be @0x950412de@ or @0xde120495@)
  , fRevision :: Word32                    -- ^ File revision
  , fSize :: Word32                        -- ^ Number of text pairs in the file
  , fOriginalOffset :: Word32              -- ^ Offset of original strings
  , fTranslationOffset :: Word32           -- ^ Offset of translations
  , fHashtableSize :: Word32               -- ^ Size of hash table
  , fHashtableOffset :: Word32             -- ^ Offset of hash table
  , fOriginals :: [(Word32, Word32)]       -- ^ Original strings - sizes and offsets
  , fTranslations :: [(Word32, Word32)]    -- ^ Translations - sizes and offsets
  , fData :: L.ByteString                  -- ^ All file data - used to access strings by offsets
  }
  deriving (Eq)

instance Show GmoFile where
  show f = printf "<GetText file size=%d>" (fSize f)

-- | This structure describes data in Gettext's @.mo/.gmo@ file in ready-to-use format.
data Catalog = Catalog {
  gmoSize :: Word32,
  gmoChoosePlural :: Int -> Int,
  gmoData :: Trie.Trie [T.Text] }

instance Show Catalog where
  show gmo = printf "<GetText data size=%d>" (gmoSize gmo)

-- | Prepare the data parsed from file for lookups.
unpackGmoFile :: GmoFile -> Catalog
unpackGmoFile (GmoFile {..}) = Catalog fSize choose trie
  where
    getOrig (len,offs) = L.take (fromIntegral len) $ L.drop (fromIntegral offs) fData

    choose = choosePluralForm' trie
    
    getTrans (len,offs) =
      let bstr = getOrig (len,offs)
      in  if L.null bstr
            then [T.empty]
            else map TLE.decodeUtf8 $ L.split 0 bstr

    originals = map L.toStrict $ map getOrig fOriginals
    translations = map getTrans fTranslations

    trie = Trie.fromList $ zip originals translations

-- | Load gettext file
loadCatalog :: FilePath -> IO Catalog
loadCatalog path = do
  content <- L.readFile path
  let gmoFile = (runGet parseGmo content) {fData = content}
  return $ unpackGmoFile gmoFile


-- | Look up for string translation
lookup :: B.ByteString -> Catalog -> Maybe [T.Text]
lookup key gmo = Trie.lookup key (gmoData gmo)

-- | Get all translation pairs
assocs :: Catalog -> [(B.ByteString, [T.Text])]
assocs = Trie.toList . gmoData

-- | Obtain headers of the catalog.
-- Headers are defined as a translation for empty string.
getHeaders :: Catalog -> Maybe Headers
getHeaders gmo = getHeaders' (gmoData gmo)

getHeaders' :: Trie.Trie [T.Text] -> Maybe Headers
getHeaders' trie =
  case Trie.lookup "" trie of
    Nothing -> Nothing
    Just texts -> either error Just $ parseHeaders (head texts)

-- | Get plural forms selection definition.
getPluralDefinition :: Catalog -> Maybe (Int, Expr)
getPluralDefinition gmo = getPluralDefinition' (gmoData gmo)

getPluralDefinition' :: Trie.Trie [T.Text] -> Maybe (Int, Expr)
getPluralDefinition' trie =
  case getHeaders' trie of
    Nothing -> Nothing
    Just headers -> either error Just $ parsePlural headers

-- | Translate a string.
-- Original message must be defined in @po@ file in @msgid@ line.
gettext :: Catalog -> B.ByteString -> T.Text
gettext gmo key =
  case lookup key gmo of
    Nothing -> TLE.decodeUtf8 $ L.fromStrict key
    Just texts -> head texts

-- | Translate a string and select correct plural form.
-- Original single form must be defined in @po@ file in @msgid@ line.
-- Original plural form must be defined in @po@ file in @msgid_plural@ line.
ngettext :: Catalog
         -> Int           -- ^ Number
         -> B.ByteString  -- ^ Single form in original language
         -> B.ByteString  -- ^ Plural form in original language
         -> T.Text
ngettext gmo n single plural = ngettext' gmo n $ single `B.append` "\0" `B.append` plural

-- | Variant of @ngettext@ for case when for some reason there is only
-- @msgid@ defined in @po@ file, and no @msgid_plural@, but there are some @msgstr[n]@.
ngettext' :: Catalog
          -> Int          -- ^ Number
          -> B.ByteString -- ^ Single form in original language
          -> T.Text
ngettext' gmo n key =
  case lookup key gmo of
    Nothing -> TLE.decodeUtf8 $ L.fromStrict key
    Just texts ->
      let plural = choosePluralForm gmo n
          idx = if plural >= length texts
                  then 0 -- if there are not enough plural forms defined, always use the first,
                         -- it is probably the most correct
                  else plural
      in  texts !! idx

-- | Choose plural form index by number
choosePluralForm :: Catalog -> Int -> Int
choosePluralForm gmo = gmoChoosePlural gmo

choosePluralForm' :: Trie.Trie [T.Text] -> Int -> Int
choosePluralForm' trie n =
  case getPluralDefinition' trie of
    Nothing -> if n == 1 then 0 else 1 -- from GNU gettext implementation, known as 'germanic plural form'
    Just (_, expr) -> eval expr n

-- | Data.Binary parser for GmoFile structure
parseGmo :: Get GmoFile
parseGmo = do
  magic <- getWord32host
  getWord32 <- case magic of
                 0x950412de -> return getWord32le
                 0xde120495 -> return getWord32be
                 _ -> fail "Invalid magic number"
  
  let getPair :: Get (Word32, Word32)
      getPair = do
        x <- getWord32
        y <- getWord32
        return (x,y)

  revision <- getWord32
  size <- getWord32
  origOffs <- getWord32
  transOffs <- getWord32
  hashSz <- getWord32
  hashOffs <- getWord32
  origs <- replicateM (fromIntegral size) getPair
  trans <- replicateM (fromIntegral size) getPair
  return $ GmoFile {
              fMagic = magic,
              fRevision = revision,
              fSize = size,
              fOriginalOffset = origOffs,
              fTranslationOffset = transOffs,
              fHashtableSize = hashSz,
              fHashtableOffset = hashOffs,
              fOriginals = origs,
              fTranslations = trans,
              fData = undefined }

withGmoFile :: FilePath -> (GmoFile -> IO a) -> IO a
withGmoFile path go = do
  content <- L.readFile path
  let gmo = (runGet parseGmo content) {fData = content}
  result <- go gmo
  return result

