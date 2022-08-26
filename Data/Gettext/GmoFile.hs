{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Data.Gettext.GmoFile
  ( -- * Data structures
    GmoFile (..),
    -- * Parsing
    parseGmo
  ) where

import Control.Monad
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import Text.Printf

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

  skip $ max 0 (fromIntegral origOffs - 28)
  origs <- replicateM (fromIntegral size) getPair

  skip $ max 0 (fromIntegral transOffs - fromIntegral origOffs - fromIntegral size * 8)
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

