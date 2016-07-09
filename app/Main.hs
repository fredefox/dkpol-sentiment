{-# language OverloadedStrings #-}
{-# language DeriveGeneric #-}

module Main where

import Sentiment (Sentiment, SentimentException)
import qualified Sentiment
import Control.Monad
import qualified Data.ByteString.Char8 as C
import Text.Printf
import qualified Data.Text as T
import GHC.Generics
import Data.Yaml (Object)
import qualified Data.Yaml
import qualified Data.Yaml.Include as Yaml
import Data.Maybe
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as BSL
import qualified Web.Twitter.Conduit as Twitter
import Web.Twitter.Types
import Control.Applicative
import Snap.Core
import Snap.Http.Server
import qualified Data.Aeson as Aeson
import Control.Monad.IO.Class

main :: IO ()
main = loadSettings >>= quickHttpServe . site

settingsPath :: String
settingsPath = "settings.yaml"

loadSettings :: IO Settings
loadSettings = do
  o <- Yaml.decodeFile settingsPath >>= maybe errMalformed return
  maybe errInvalid return $ toSettings o
    where
      errMalformed = error "Malformed configuration file"
      errInvalid   = error "Invalid configuration file"
      -- TODO: Report any keys that might be missing
      toSettings :: Object -> Maybe Settings
      toSettings o = do
        sentKey  <- "sentiment-api-key"       `lkpString` o
        sentEndp <- "sentiment-endpoint"      `lkpString` o
        twCKey   <- "twitter-consumer-key"    `lkpString` o
        twCSec   <- "twitter-consumer-secret" `lkpString` o
        twATok   <- "twitter-access-token"    `lkpString` o
        twASec   <- "twitter-access-secret"   `lkpString` o
        return Settings
          { apiKey     = sentKey
          , endpoint   = sentEndp
          , twConsKey  = twCKey
          , twConsSecr = twCSec
          , twAccTok   = twATok
          , twAccSecr  = twASec
          }
      lkpString :: T.Text -> Object -> Maybe String
      k `lkpString` o = k `HashMap.lookup` o >>= valToString
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
    , twConsKey :: String
    , twConsSecr :: String
    , twAccTok :: String
    , twAccSecr :: String
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

settingsToTwInfo :: Settings -> Twitter.TWInfo
settingsToTwInfo s = Twitter.TWInfo tok Nothing
  where
    tok = Twitter.TWToken oauth cred
    oauth = Twitter.twitterOAuth
      { Twitter.oauthConsumerKey = S8.pack . twConsKey $ s
      , Twitter.oauthConsumerSecret = S8.pack . twConsSecr $ s
      }
    cred = Twitter.Credential
      [ ("oauth_token", S8.pack . twAccTok $ s)
      , ("oauth_token_secret", S8.pack . twAccSecr $ s)
      ]

getSentimentsFromTwitter :: Settings -> T.Text -> IO (Either SentimentException [(SearchStatus, Sentiment)])
getSentimentsFromTwitter s qry = do
    res <- getTwitterPosts (settingsToTwInfo s) qry
    let statuses = searchResultStatuses res
    eitherErrOrRes <- sentFromSearch s statuses
    return $ do
        sentiments <- eitherErrOrRes
        return $ zip statuses sentiments

sentFromSearch :: Settings -> [SearchStatus] -> IO (Either SentimentException [Sentiment])
sentFromSearch s st = Sentiment.sentiment url key (map f st)
  where
    f :: SearchStatus -> S8.ByteString
    f = S8.pack . T.unpack . searchStatusText
    url = endpoint s
    key = apiKey s

getTwitterPosts
    :: Twitter.TWInfo
    -> T.Text
    -> IO (SearchResult [SearchStatus])
getTwitterPosts nfo s = do
    mgr <- Twitter.newManager Twitter.tlsManagerSettings
    Twitter.call nfo mgr . Twitter.search $ s

site :: Settings -> Snap ()
site s = modifyResponse (setHeader "Access-Control-Allow-Origin" "*") >> route
  [ ("sentiments/:query", sentimentsHandler s)
  ]

sentimentsHandler :: Settings -> Snap ()
sentimentsHandler s = do
    param <- getParam "query"
    maybe (writeBS "must specify echo/param in URL") performSearch param
      where
        performSearch :: S8.ByteString -> Snap ()
        performSearch qry = liftIO >=> writeBS $ do
          resp <- getSentimentsFromTwitter s . T.pack . S8.unpack $ qry
          return . BSL.toStrict
            $ either (const "oh's to da no's") Aeson.encode resp
