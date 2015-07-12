{-# LANGUAGE DeriveGeneric #-}
module Twitter.Tweet where

import IRC.Types hiding (name)
import Twitter.Auth
import GHC.Generics
import Data.Text
import Data.Aeson

data TwitterUser = TwitterUser { name :: !Text, screen_name :: !Text } deriving (Generic)
instance FromJSON TwitterUser
instance ToJSON TwitterUser
instance Show TwitterUser where
  show user' = unpack (name user') ++ " (@" ++ unpack (screen_name user') ++ ")"

data Tweet = Tweet { text :: !Text, user :: !TwitterUser } deriving (Generic)
instance FromJSON Tweet
instance ToJSON Tweet
instance Show Tweet where
  show tweet = show (user tweet) ++ ": " ++ unpack (text tweet)

readTweet :: String -> IRC (Either String Tweet)
readTweet id' = do
  req <- authRequest ("https://api.twitter.com/1.1/statuses/show.json?id=" ++ id')
  return $ case req of
    Left err -> Left err
    Right r -> eitherDecode r
