-- Originally copied from https://github.com/vasylp/hgettext/
-- © 2009 Vasyl Pasternak, BSD-3-Clause.

import Options
import Paths_haskell_gettext (version)

import qualified Data.Generics.Uniplate.Data as G
import qualified Data.Function as F
import qualified Data.List as L
import qualified Data.Ord as O
import qualified Data.Text as T
import qualified Data.Time as TM
import qualified Data.Text.IO.Utf8 as Utf8
import qualified Data.Version as V
import qualified Language.Haskell.Exts as H


main :: IO ()
main = do
    opts <- parseOptions
    process opts

process :: Options -> IO ()
process Options{printVersion = True} =
        putStrLn $ "hgettext (from haskell-gettext), version " ++
                     (V.showVersion version)
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

-------------------------------------------------------------------------------
-- Write


formatPotFile :: [String] -> IO String
formatPotFile ls = do
    time <- TM.getZonedTime
    let timeStr = TM.formatTime TM.defaultTimeLocale "%F %R%z" time
    let header = formatPotHeader timeStr
    return $ concat (header:ls)
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

-- Create list of messages from a single file.
formatMessages :: String -> [(H.SrcSpanInfo, String)] -> String
formatMessages path l =
        let sorted = L.sortBy (O.comparing snd) l
            nubbed = L.nubBy ((==) `F.on` snd) sorted
        in concatMap potEntry nubbed
    where
        potEntry :: (H.SrcSpanInfo, String) -> String
        potEntry (wl, s) = unlines
                             ["#: " ++ showSrc wl,
                              "msgid " ++ (showStringC s),
                              "msgstr \"\"",
                              ""]

        showSrc :: H.SrcSpanInfo -> String
        showSrc wl = path ++ ":" ++
                     show (H.srcSpanStartLine (H.srcInfoSpan wl)) ++ ":" ++
                     show (H.srcSpanStartColumn (H.srcInfoSpan wl))

toTranslate :: [String] -> H.ParseResult (H.Module H.SrcSpanInfo) ->
               [(H.SrcSpanInfo, String)]
toTranslate f (H.ParseOk z) =
        L.nub [(loc, s) |
                H.App _ (H.Var _ (H.UnQual _ (H.Ident _ x)))
                        (H.Lit _ (H.String loc s _)) <- G.universeBi z,
                x `elem` f]
toTranslate _ _ = []

-- Escape a string in a C-like fashion,
-- see https://www.ibm.com/docs/en/i/7.4?topic=literals-string
showStringC :: String -> String
showStringC s0 = '"' : concatMap showCharC s0 ++ "\""
    where
      showCharC '"' = "\\\""
      showCharC '\\' = "\\\\"
      showCharC '\n' = "\\n"
      showCharC c = return c
