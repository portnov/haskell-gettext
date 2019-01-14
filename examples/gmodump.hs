
import Data.Gettext

import Control.Monad
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Lazy.IO as TLIO
import System.Environment

main = do
  [file] <- getArgs
  catalog <- loadCatalog file
  case getPluralDefinition catalog of
    Nothing -> putStrLn "No plural forms selection expression provided."
    Just (_,expr) -> putStrLn $ "Plural forms selection: " ++ show expr
  forM_ (assocs catalog) $ \(orig,trans) -> do
    putStr "Original: "
    B8.putStrLn orig
    forM_ (zip [0..] trans) $ \(i,tran) -> do
      putStr $ "Translation #" ++ show i ++ ": "
      TLIO.putStrLn tran
