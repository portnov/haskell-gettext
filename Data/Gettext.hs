{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
-- | This is the main module of @haskell-gettext@ package.
-- For most cases, it is enough to import only this module. 
-- Other modules of the package might be useful for other libraries
-- working with gettext's files.
--
-- Simple example of usage of this module is:
--
-- @
-- {-\# LANGUAGE OverloadedStrings #\-}
-- module Main where
--
-- import qualified Data.Text.Lazy as T
-- import qualified Text.Lazy.IO as TLIO
-- import Text.Printf
--
-- import Data.Gettext
--
-- main :: IO ()
-- main = do
--   catalog <- loadCatalog "locale\/ru\/messages.mo"
--   TLIO.putStrLn $ gettext catalog "Simple translated message"
--   let n = 78
--   let template = ngettext catalog "There is %d file" "There are %d files" n
--   printf (T.unpack template) n
-- @
--
module Data.Gettext
  ( -- * Data structures
   Catalog,
   -- * Loading and using translations
   loadCatalog,
   lookup,
   gettext, cgettext,
   ngettext, cngettext,
   ngettext',
   context,
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
import qualified Data.Map as M
import Text.Printf

import Data.Gettext.GmoFile
import Data.Gettext.Plural
import Data.Gettext.Parsers

-- | This structure describes data in Gettext's @.mo/.gmo@ file in ready-to-use format.
data Catalog = Catalog {
  gmoSize :: Word32,
  gmoChoosePlural :: Int -> Int,
  gmoData :: M.Map B.ByteString [T.Text] }

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
lookup key gmo = M.lookup key (gmoData gmo)

-- | Get all translation pairs
assocs :: Catalog -> [(B.ByteString, [T.Text])]
assocs = M.assocs . gmoData

-- | Obtain headers of the catalog.
-- Headers are defined as a translation for empty string.
getHeaders :: Catalog -> Maybe Headers
getHeaders gmo = getHeaders' (gmoData gmo)

getHeaders' :: M.Map B.ByteString [T.Text] -> Maybe Headers
getHeaders' trie =
  case M.lookup "" trie of
    Nothing -> Nothing
    Just texts -> either error Just $ parseHeaders (head texts)

-- | Get plural forms selection definition.
getPluralDefinition :: Catalog -> Maybe (Int, Expr)
getPluralDefinition gmo = getPluralDefinition' (gmoData gmo)

getPluralDefinition' :: M.Map B.ByteString [T.Text] -> Maybe (Int, Expr)
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

-- | Get sub-catalog for specific context
context :: Catalog
        -> B.ByteString -- ^ Context
        -> Catalog
context gmo ctxt =
  let n = B.length ctxt + 1
      prefix = ctxt `B.append` "\4"
      trie =
        M.fromList [(B.drop n key, value) | (key, value) <- M.assocs (gmoData gmo), prefix `B.isPrefixOf` key]
  in  Catalog {
        gmoSize = fromIntegral (M.size trie),
        gmoChoosePlural = gmoChoosePlural gmo,
        gmoData = trie }

-- | Choose plural form index by number
choosePluralForm :: Catalog -> Int -> Int
choosePluralForm gmo = gmoChoosePlural gmo

choosePluralForm' :: M.Map B.ByteString [T.Text] -> Int -> Int
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

    trie = M.fromList $ zip originals translations

