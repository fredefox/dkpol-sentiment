{-# language OverloadedStrings #-}

module Main where

import System.Environment
import Sentiment
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = getArgs >>= sentiment' >>= printSentiments

sentiment' :: [String] -> IO (Either SentimentException [Sentiment])
sentiment' = sentiment endpoint apiKey . map C.pack

apiKey :: String
apiKey = error "Enter credentials"

endpoint :: String
--endpoint = "http://localhost:1337"
endpoint = "https://ussouthcentral.services.azureml.net/workspaces/c788c20533a549f78f520f17b4b17f39/services/fe5d1e3a57a04236892b69f36a0de3ef/execute?api-version=2.0&details=true"

printSentiments :: Either SentimentException [Sentiment] -> IO ()
printSentiments = print
