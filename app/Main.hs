module Main (main) where

import Lipsum
import Options (amount, optionsParser, start, textUnit)
import Options.Applicative (execParser)

main :: IO ()
main = do
  options <- execParser optionsParser
  result <- getText (textUnit options) (amount options) (start options)
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right response -> do
      putStrLn $ text response
