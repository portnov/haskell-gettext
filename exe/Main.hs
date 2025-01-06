-- (C)  vasylp https://github.com/vasylp/hgettext/blob/master/src/hgettext.hs

import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as Utf8
import qualified Language.Haskell.Exts as H

import Options

import Data.Time

import Data.Generics.Uniplate.Data

-- import Distribution.Simple.PreProcess.Unlit

import Data.List
import Data.Ord
import Data.Function (on)

import Data.Version (showVersion)
import Paths_haskell_gettext (version)

toTranslate :: [String] -> H.ParseResult (H.Module H.SrcSpanInfo) -> [(H.SrcSpanInfo, String)]
toTranslate f (H.ParseOk z) = nub [ (loc, s) | H.App _ (H.Var _ (H.UnQual _ (H.Ident _ x))) (H.Lit _ (H.String loc s _)) <- universeBi z, x `elem` f]
toTranslate _ _ = []

-- Create list of messages from a single file
formatMessages :: String -> [(H.SrcSpanInfo, String)] -> String
formatMessages path l = concat $ map potEntry $ nubBy ((==) `on` snd) $ sortBy (comparing snd) l
    where potEntry (wl, s) = unlines [
                             "#: " ++ showSrc wl,
                             "msgid " ++ (showStringC s),
                             "msgstr \"\"",
                             ""
                            ]
          showSrc wl = path ++ ":" ++ show (H.srcSpanStartLine (H.srcInfoSpan wl)) ++ ":" ++ show (H.srcSpanStartColumn (H.srcInfoSpan wl))

-- Escape a string in a C-like fashion,
-- see https://www.ibm.com/docs/en/i/7.4?topic=literals-string
showStringC :: String -> String
showStringC s0 = '"' : concatMap showCharC s0 ++ "\""
    where
      showCharC '"' = "\\\""
      showCharC '\\' = "\\\\"
      showCharC '\n' = "\\n"
      showCharC c = return c


formatPotFile :: [String] -> IO String
formatPotFile ls = do
    time <- getZonedTime
    let timeStr = formatTime defaultTimeLocale "%F %R%z" time
    let header = formatPotHeader timeStr
    return $ concat $ header: ls
  where
    formatPotHeader :: String -> String
    formatPotHeader timeStr =
       unlines ["# Translation file",
                "",
                "msgid \"\"",
                "msgstr \"\"",
                "",
                "\"Project-Id-Version: PACKAGE VERSION\\n\"",
                "\"Report-Msgid-Bugs-To: \\n\"",
                "\"POT-Creation-Date: " ++ timeStr ++ "\\n\"",
                "\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n\"",
                "\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"",
                "\"Language-Team: LANGUAGE <LL@li.org>\\n\"",
                "\"MIME-Version: 1.0\\n\"",
                "\"Content-Type: text/plain; charset=UTF-8\\n\"",
                "\"Content-Transfer-Encoding: 8bit\\n\"",
                ""]

process :: Options -> IO ()
process Options{printVersion = True} =
        putStrLn $ "hgettext (from haskell-gettext), version " ++
                     (showVersion version)
process opts
    | null (inputFiles opts) = do
        putStrLn "hgettext: missing arguments"
    | otherwise = do
        t <- mapM read' (inputFiles opts)
        pot <- formatPotFile $
                 map (\(n,c) -> formatMessages n $
                                  toTranslate (keywords opts) c) t
        Utf8.writeFile (outputFile opts) (T.pack pot)
    where
        read' :: FilePath ->
                 IO (String, H.ParseResult (H.Module H.SrcSpanInfo))
        read' "-" = getContents >>= \c -> return ("-", H.parseFileContents c)
        read' f = H.parseFile f >>= \m -> return (f, m)

main :: IO ()
main = do
    opts <- parseOptions
    process opts
