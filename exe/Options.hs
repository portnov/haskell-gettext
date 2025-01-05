module Options (
        Options(..),
        parseOptions
    )
        where

import Options.Applicative

import qualified System.FilePath as FP

data Options = Options {
      inputFiles :: [FilePath],
      outputFile :: FilePath,
      keywords :: [String],
      printVersion :: Bool
    } deriving Show

parseOptions :: IO Options
parseOptions = execParser infoOpts

-------------------------------------------------------------------------------
-- Parsers/properties

infoOpts :: ParserInfo Options
infoOpts = info (options <**> helper)
  ( fullDesc
  <> progDesc "Extract translatable strings from Haskell source files."
  <> header "hello - a test for optparse-applicative" )

options :: Parser Options
options = Options <$> inputs <*> outfile <*> kwsDef <*> version
    where
        kwsDef = ("__" :) <$> many keyword

inputs :: Parser [FilePath]
inputs = many (strArgument (metavar "PATH..."))

outfile :: Parser FilePath
outfile = output <|> ((FP.<.> "po") <$> defaultDomain) <|> pure "messages.po"
    where
        output :: Parser FilePath
        output = strOption
            ( long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "write output to specified file" )

        defaultDomain :: Parser FilePath
        defaultDomain = strOption
            ( long "default-domain"
            <> short 'd'
            <> metavar "NAME"
            <> help "use NAME.po instead of messages.po" )

keyword :: Parser String
keyword = strOption
                ( long "keyword"
                <> short 'k'
                <> metavar "WORD"
                <> help "function names, in which searched words are \
                        \wrapped. Can be used multiple times, for multiple \
                        \funcitons" )

version :: Parser Bool
version = switch
            ( long "version"
            <> short 'v'
            <> help "print version of hgettext" )
