{-# language OverloadedStrings #-}

module Sentiment
    ( sentiment
    , Sentiment
    , SentimentException
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception
import Data.Typeable (Typeable)
import Network.HTTP.Conduit
import Network.HTTP.Simple (setRequestIgnoreStatus)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Char8 as C
import Network.HTTP.Types.Status
import Data.Aeson
import Control.Lens
import Data.Aeson.Lens
import qualified Data.HashMap.Strict as M
import Data.Aeson.Types
import qualified Data.Vector as V
import Data.Scientific (Scientific)
import Network.HTTP.Types

sentiment
    :: String
    -> String
    -> [B.ByteString]
    -> IO (Either SentimentException [Sentiment])
sentiment endpoint apiKey txts = do
    mgr <- newManager tlsManagerSettings
    runResourceT $ querySentiment mgr endpoint apiKey txts

querySentiment
    :: Manager
    -> String
    -> String
    -> [B.ByteString]
    -> ResourceT IO (Either SentimentException [Sentiment])
querySentiment mgr endpoint apiKey txts
  = either (return . Left . OtherException) mkSentiment $ mkRequest endpoint apiKey txts
      where
        mkSentiment
          :: Request
          -> ResourceT IO (Either SentimentException [Sentiment])
        mkSentiment req = sentimentFromResponse txts' <$> httpLbs req mgr
        txts' = map (T.pack . C.unpack) txts

sentimentFromResponse
    :: [Text]
    -> Response BL.ByteString
    -> Either SentimentException [Sentiment]
sentimentFromResponse txts resp
    | statusIsSuccessful . responseStatus $ resp
        = parseSentiments txts . responseBody $ resp
    | otherwise
        = Left . GenericException . CL.unpack . responseBody $ resp

parseSentiments :: [Text] -> BL.ByteString -> Either SentimentException [Sentiment]
parseSentiments txts bs = maybe (Left malformedJson) (Right . obj2sent) . decode $ bs
  where
    obj2sent :: Object -> [Sentiment]
    -- The query we need is:
    --   v .: "Results" .: "output1" .: "value"
    --     .: "values"sults" .: "output1" .: "value" .: "values"
    obj2sent obj = maybe (error "plz no") sents (parseMaybe (\o -> o .: "Results" >>= (.: "output1") >>= (.: "value") >>= (.: "Values")) obj::Maybe Array)
    sents :: Array -> [Sentiment]
    sents = map valToSent . zip txts . V.toList
    valToSent :: (Text, Value) -> Sentiment
    valToSent (qry, Array a) = Sentiment { label = valToTxt $ a V.! 0, conf = valToDouble $ a V.! 1, query = qry }
    valToSent _ = error "no, no no!"
    malformedJson = undefined
    invalidJson :: SentimentException
    invalidJson = InvalidResponse (-1) "ParseError"
        "Could not parse result JSON from endpoint"

valToTxt :: Value -> Text
valToTxt (String t) = t
valToTxt _ = error "no parse"

valToDouble :: Value -> Double
valToDouble (String num) = read $ T.unpack num
valToDouble _ = error "no parse"

mkRequest
    :: String
    -> String
    -> [B.ByteString]
    -> Either SomeException Request
mkRequest url apiKey txts = modifyRequest <$> parseUrlSafe url
  where
    parseUrlSafe = fmap setRequestIgnoreStatus . parseUrl
    modifyRequest req = req
      { method = methodPost
      , requestHeaders =
        [ ("Authorization", B.concat ["Bearer ", C.pack apiKey])
        , ("Content-Type", "application/json")
        ]
      , requestBody = RequestBodyBS $ encodeParams txts
      }

encodeParams :: [C.ByteString] -> C.ByteString
encodeParams txts = C.pack . CL.unpack . encode $ object
  [ ("Inputs", object
    [ ("input1", object
      [ ("ColumnNames", Array $ V.fromList ["tweet_text"])
      , ("Values", Array $ V.fromList $ map (Array . V.singleton . String . T.pack . C.unpack) txts) ]
      )
    ]
    )  
  ]

data SentimentException = InvalidResponse Int Text Text
    | OtherException SomeException
    | GenericException String
    deriving (Show, Typeable)

instance Exception SentimentException

data Sentiment = Sentiment
    { label :: Text
    , conf :: Double
    , query :: Text
    } deriving (Show)
