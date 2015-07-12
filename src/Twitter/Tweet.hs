{-# LANGUAGE DeriveGeneric #-}
module Twitter.Tweet where

import IRC.Types
import Twitter.Auth
import GHC.Generics
import Data.Text
import Data.Aeson

data Tweet = Tweet { text :: !Text } deriving (Show, Generic)
instance FromJSON Tweet
instance ToJSON Tweet

readTweet :: String -> IRC (Either String Tweet)
readTweet id' = do
  req <- authRequest ("https://api.twitter.com/1.1/statuses/show.json?id=" ++ id')
  return $ case req of
    Left err -> Left err
    Right r -> eitherDecode r
