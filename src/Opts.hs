module Opts (Opts (..), optionsParser, TextUnit (..)) where

import Options.Applicative

data Opts = Opts
  { textUnit :: !TextUnit,
    amount :: !Int,
    start :: !Bool
  }
  deriving (Eq, Show)

data TextUnit = Paragraphs | Words | Bytes deriving (Eq, Enum)

instance Read TextUnit where
  readsPrec _ val =
    tryParse [("paras", Paragraphs), ("words", Words), ("bytes", Bytes)]
    where
      tryParse [] = []
      tryParse ((attempt, result) : xs) =
        if take (length attempt) val == attempt
          then [(result, drop (length attempt) val)]
          else tryParse xs

instance Show TextUnit where
  show Paragraphs = "paras"
  show Words = "words"
  show Bytes = "bytes"

optionsParser :: ParserInfo Opts
optionsParser =
  info
    (helper <*> versionOption <*> programOptions)
    (fullDesc <> progDesc "Get some 'Lorem ipsum' text" <> header "lipsum - a CLI to the Lipsum API (https://lipsum.com)")

versionOption :: Parser (a -> a)
versionOption = infoOption "0.0.0" (long "version" <> short 'V' <> help "Show version")

programOptions :: Parser Opts
programOptions =
  Opts
    <$> option auto (long "unit" <> short 'u' <> metavar "UNIT" <> value Paragraphs <> help "Set the text unit (paras (default), words, or bytes)")
    <*> option auto (long "amount" <> short 'a' <> metavar "AMOUNT" <> help "Set the amount of text units")
    <*> switch (long "start" <> short 's' <> help "Do not start with 'Lorem ipsum'")
