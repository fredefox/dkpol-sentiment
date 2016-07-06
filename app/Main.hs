{-# language OverloadedStrings #-}
{-# language DeriveGeneric #-}

module Main where

import System.Environment
import Sentiment (Sentiment, SentimentException)
import qualified Sentiment
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Text.Printf
import qualified Data.List as L
import qualified Data.Text as T
import GHC.Generics
import Data.Yaml (Object)
import qualified Data.Yaml
import qualified Data.Yaml.Include as Yaml
import Data.Maybe
import qualified Data.HashMap.Strict as HashMap

main :: IO ()
main = do
  settings <- loadSettings settingsPath
  getContents
    >>= sentiment settings . lines
    >>= either print (`forM_` printSentiment)

settingsPath :: String
settingsPath = "settings.yaml"

loadSettings :: FilePath -> IO Settings
loadSettings p = do
  o <- fromMaybe errMalformed <$> Yaml.decodeFile p
  maybe errInvalid return $ toSettings o
    where
      errMalformed = error "Malformed configuration file"
      errInvalid   = error "Invalid configuration file"
      toSettings :: Object -> Maybe Settings
      toSettings o = do
        k <- "api-key"  `lkpString` o
        e <- "endpoint" `lkpString` o
        return Settings { apiKey = k, endpoint = e }
      lkpString :: T.Text -> Object -> Maybe String
      k `lkpString` o = do
        v <- k `HashMap.lookup` o
        valToString v
      valToString :: Data.Yaml.Value -> Maybe String
      valToString (Data.Yaml.String s) = Just . T.unpack $ s
      valToString _ = Nothing

sentiment :: Settings -> [String] -> IO (Either SentimentException [Sentiment])
sentiment settings = Sentiment.sentiment endpoint' apiKey' . map C.pack
  where
    endpoint' = endpoint settings
    apiKey' = apiKey settings

data Settings = Settings
    { apiKey :: String
    , endpoint :: String
    } deriving (Generic, Show)

printSentiment :: Sentiment -> IO ()
printSentiment s = printf "[%s (%.2f%%)] %s\n"
    (Sentiment.label s)
    (Sentiment.conf s * 100)
    (ellipsis . map spaces . T.unpack . Sentiment.query $ s)

spaces :: Char -> Char
spaces c
    | c == '\n' = ' '
    | otherwise = c

ellipsis :: String -> String
ellipsis s
    | null . take 80 $ s = s
    | otherwise          = (++ "...") . take 77 $ s
