{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import System.FilePath
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TLIO
import System.IO
import System.Environment
import Text.Printf

import Data.Gettext

main :: IO ()
main = do
  [file, ns] <- getArgs
  let n = read ns
  catalog <- loadCatalog file
  let localizedTemplate = ngettext catalog n "There is %d file" "There are %d files"
  TLIO.putStrLn localizedTemplate
  printf (T.unpack localizedTemplate) n

