{-# LANGUAGE RecordWildCards #-}

module Data.Gettext
  ( -- * Data structures
   GmoFile (..),
   GmoData,
   -- * Loading and using translations
   loadGmoData,
   lookup,
   -- * Utilities for custom parsers implementation
   parseGmo,
   unpackGmoFile
  ) where

import Prelude hiding (lookup)
import Control.Monad
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
  show f = printf "<GMO file size=%d>" (fSize f)

-- | This structure describes data in Gettext's @.mo/.gmo@ file in ready-to-use format.
data GmoData = GmoData {
  gmoSize :: Word32,
  gmoData :: Trie.Trie [T.Text] }
  deriving (Eq)

instance Show GmoData where
  show gmo = printf "<GMO data size=%d>" (gmoSize gmo)

-- | Prepare the data parsed from file for lookups.
unpackGmoFile :: GmoFile -> GmoData
unpackGmoFile (GmoFile {..}) = GmoData fSize trie
  where
    getOrig (len,offs) = L.take (fromIntegral len) $ L.drop (fromIntegral offs) fData
    
    getTrans (len,offs) =
      let bstr = getOrig (len,offs)
      in  if L.null bstr
            then [T.empty]
            else map TLE.decodeUtf8 $ L.split 0 bstr

    originals = map L.toStrict $ map getOrig fOriginals
    translations = map getTrans fTranslations

    trie = Trie.fromList $ zip originals translations

-- | Load gettext file
loadGmoData :: FilePath -> IO GmoData
loadGmoData path = do
  content <- L.readFile path
  let gmoFile = (runGet parseGmo content) {fData = content}
  return $ unpackGmoFile gmoFile

-- | Look up for string translation
lookup :: B.ByteString -> GmoData -> Maybe [T.Text]
lookup key gmo = Trie.lookup key (gmoData gmo)

-- | Data.Binary parser for GmoFile structure
parseGmo :: Get GmoFile
parseGmo = do
  magic <- getWord32host
  if magic /= 0x950412de && magic /= 0xde120495
    then fail "Invalid magic number"
    else do
         revision <- getWord32host
         size <- getWord32host
         origOffs <- getWord32host
         transOffs <- getWord32host
         hashSz <- getWord32host
         hashOffs <- getWord32host
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

getPair :: Get (Word32, Word32)
getPair = do
  x <- getWord32host
  y <- getWord32host
  return (x,y)

withGmoFile :: FilePath -> (GmoFile -> IO a) -> IO a
withGmoFile path go = do
  content <- L.readFile path
  let gmo = (runGet parseGmo content) {fData = content}
  result <- go gmo
  return result

