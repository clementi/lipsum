{-# LANGUAGE OverloadedStrings #-}

module Lipsum
  ( getText,
    LipsumResponse (text, summary),
  )
where

import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Conduit
import Options (TextUnit (Bytes, Paragraphs, Words))

data LipsumResponse = LipsumResponse {text :: String, summary :: String} deriving (Show)

(.->) :: FromJSON b => Parser Object -> Key -> Parser b
(.->) parser key = do
  obj <- parser
  obj .: key

instance FromJSON LipsumResponse where
  parseJSON = withObject "LipsumResponse" $ \obj -> do
    responseText <- obj .: "feed" .-> "lipsum"
    responseSummary <- obj .: "feed" .-> "generated"
    return (LipsumResponse {text = responseText, summary = responseSummary})

getText :: TextUnit -> Int -> Bool -> IO (Either String LipsumResponse)
getText textUnit amount start = do
  manager <- newManager tlsManagerSettings
  let url = buildUrl textUnit amount start
  request <- parseRequest url
  response <- httpLbs request manager
  let body = responseBody response
  let decoded = eitherDecode body :: Either String LipsumResponse
  return decoded

buildUrl :: TextUnit -> Int -> Bool -> String
buildUrl textUnit amount start =
  "https://lipsum.com/feed/json?what=" ++ toQueryParam textUnit ++ "&amount=" ++ show amount ++ "&start=" ++ yesno start

toQueryParam :: TextUnit -> String
toQueryParam Paragraphs = "paras"
toQueryParam Words = "words"
toQueryParam Bytes = "bytes"

yesno :: Bool -> String
yesno True = "yes"
yesno _ = "no"