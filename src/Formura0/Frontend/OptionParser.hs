module Formura0.Frontend.OptionParser
  ( getOption
  , getOptionPure
  ) where

import Options.Applicative

import Formura0.Option

getOption :: IO Option
getOption = execParser optParserInfo

-- | テスト用の関数
getOptionPure :: [String] -> Either String Option
getOptionPure args =
  case execParserPure defaultPrefs optParserInfo args of
    Success opt           -> Right opt
    Failure err           -> Left ("fail: " ++ show err)
    CompletionInvoked res -> Left ("comp: " ++ show res)

optParserInfo :: ParserInfo Option
optParserInfo =
  info (optParser <**> helper)
       (fullDesc <>
        progDesc "generate c program from formura program." <>
        header "formura - a domain-specific language for stencil computation")

optParser :: Parser Option
optParser = Option <$> strArgument (metavar "FILENAME" <> help "a file name of formura program")
                   <*> switch (long "verbose" <> help "show detail report")
                   <*> maybeOption null (long "nc"
                                        <> value ""
                                        <> metavar "FILENAME"
                                        <> help "the name of the file that provides configuration in YAML format.")
  where
    maybeOption cond mods = fmap (\s -> if cond s then Nothing else Just s) (strOption mods)
