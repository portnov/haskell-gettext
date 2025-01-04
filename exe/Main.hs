-- (C)  vasylp https://github.com/vasylp/hgettext/blob/master/src/hgettext.hs

import qualified Language.Haskell.Exts as H 

import System.Environment
import System.Console.GetOpt
import Data.Time
import System.Locale hiding (defaultTimeLocale)

import Data.Generics.Uniplate.Data

-- import Distribution.Simple.PreProcess.Unlit

import Data.List
import Data.Char
import Data.Ord
import Data.Function (on)
import System.FilePath

import Data.Version (showVersion)
version = undefined
-- import Paths_haskell_gettext (version)

data Options = Options {
      outputFile :: String,
      keywords :: [String],
      printVersion :: Bool
    } deriving Show

options :: [OptDescr (Options->Options)]
options = 
    [
     Option ['o'] ["output"] 
                (ReqArg (\o opts -> opts {outputFile = o}) "FILE") 
                "write output to specified file",
     Option ['d'] ["default-domain"] 
            (ReqArg (\d opts -> opts {outputFile = d ++ ".po"}) "NAME")
            "use NAME.po instead of messages.po",
     Option ['k'] ["keyword"] 
            (ReqArg (\d opts -> opts {keywords = d: keywords opts}) "WORD")
            "function names, in which searched words are wrapped. Can be used multiple times, for multiple funcitons",
     Option [] ["version"]
            (NoArg (\opts -> opts {printVersion = True}))
            "print version of hgettexts"
    ]


defaultOptions = Options "messages.po" ["__", "lprintf"] False

parseArgs :: [String] -> IO (Options, [String])
parseArgs args = 
    case getOpt Permute options args of
      (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
      (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: hgettext [OPTION] [INPUTFILE] ..."


toTranslate :: [String] -> H.ParseResult (H.Module H.SrcSpanInfo) -> [(H.SrcSpanInfo, String)]
toTranslate f (H.ParseOk z) = nub [ (loc, s) | H.App _ (H.Var _ (H.UnQual _ (H.Ident _ x))) (H.Lit _ (H.String loc s _)) <- universeBi z, x `elem` f]
toTranslate _ _ = []

-- Create list of messages from a single file
formatMessages :: String -> [(H.SrcSpanInfo, String)] -> String
formatMessages path l = concat $ map potEntry $ nubBy ((==) `on` snd) $ sortBy (comparing snd) l
    where potEntry (l, s) = unlines [
                             "#: " ++ showSrc l,
                             "msgid " ++ (show s),
                             "msgstr \"\"",
                             ""
                            ]
          showSrc l = path ++ ":" ++ show (H.srcSpanStartLine (H.srcInfoSpan l)) ++ ":" ++ show (H.srcSpanStartColumn (H.srcInfoSpan l))


formatPotFile :: [String] -> IO String
formatPotFile lines = do
    time <- getZonedTime
    let timeStr = formatTime defaultTimeLocale "%F %R%z" time
    let header = formatPotHeader timeStr
    return $ concat $ header: lines
  where
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

process :: Options -> [String] -> IO ()
process Options{printVersion = True} _ = 
    putStrLn $ "hgettext, version " ++ (showVersion version)

process opts fl = do
  t <- mapM read' fl
  pot <- formatPotFile $ map (\(n,c) -> formatMessages n $ toTranslate (keywords opts) c) t
  writeFile (outputFile opts) pot
    where read' "-" = getContents >>= \c -> return ("-", H.parseFileContents c)
          read' f = H.parseFile f >>= \m -> return (f, m)

main = 
    getArgs >>= parseArgs >>= uncurry process


