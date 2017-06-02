{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Data.Gettext
  ( -- * Data structures
   Catalog,
   -- * Loading and using translations
   loadCatalog,
   lookup,
   gettext, cgettext,
   ngettext, cngettext,
   ngettext',
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
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Trie as Trie
import Text.Printf

import Data.Gettext.GmoFile
import Data.Gettext.Plural
import Data.Gettext.Parsers

-- | This structure describes data in Gettext's @.mo/.gmo@ file in ready-to-use format.
data Catalog = Catalog {
  gmoSize :: Word32,
  gmoChoosePlural :: Int -> Int,
  gmoData :: Trie.Trie [T.Text] }

instance Show Catalog where
  show gmo = printf "<GetText data size=%d>" (gmoSize gmo)

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
gettext :: Catalog
        -> B.ByteString -- ^ Original string
        -> T.Text
gettext gmo key =
  case lookup key gmo of
    Nothing -> TLE.decodeUtf8 $ L.fromStrict key
    Just texts -> head texts

-- | Translate a string within specific context.
cgettext :: Catalog
         -> B.ByteString -- ^ Message context (@msgctxt@ line in @po@ file)
         -> B.ByteString -- ^ Original string
         -> T.Text
cgettext gmo context key = gettext gmo (context `B.append` "\4" `B.append` key)

-- | Translate a string and select correct plural form.
-- Original single form must be defined in @po@ file in @msgid@ line.
-- Original plural form must be defined in @po@ file in @msgid_plural@ line.
ngettext :: Catalog
         -> B.ByteString  -- ^ Single form in original language
         -> B.ByteString  -- ^ Plural form in original language
         -> Int           -- ^ Number
         -> T.Text
ngettext gmo single plural n = ngettext' gmo (single `B.append` "\0" `B.append` plural) n

-- | Translate a string and select correct plural form, within specific context
-- Original single form must be defined in @po@ file in @msgid@ line.
-- Original plural form must be defined in @po@ file in @msgid_plural@ line.
cngettext :: Catalog
         -> B.ByteString  -- ^ Message context (@msgctxt@ line in @po@ file)
         -> B.ByteString  -- ^ Single form in original language
         -> B.ByteString  -- ^ Plural form in original language
         -> Int           -- ^ Number
         -> T.Text
cngettext gmo context single plural n =
  ngettext' gmo (context `B.append` "\4" `B.append` single `B.append` "\0" `B.append` plural) n

-- | Variant of @ngettext@ for case when for some reason there is only
-- @msgid@ defined in @po@ file, and no @msgid_plural@, but there are some @msgstr[n]@.
ngettext' :: Catalog
          -> B.ByteString -- ^ Single form in original language
          -> Int          -- ^ Number
          -> T.Text
ngettext' gmo key n =
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

