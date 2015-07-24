{-# LANGUAGE DeriveGeneric #-}
module Modules.Twitter.Tweet where

import Core.IRC.Types hiding (name)
import Modules.Twitter.Auth
import GHC.Generics
import Data.Text
import Data.Aeson

data TwitterUrl = TwitterUrl { url :: !Text, expanded_url :: !Text } deriving (Generic)
instance FromJSON TwitterUrl
instance ToJSON TwitterUrl

data TwitterEntities = TwitterEntities { urls :: ![TwitterUrl] } deriving (Generic)
instance FromJSON TwitterEntities
instance ToJSON TwitterEntities

data TwitterUser = TwitterUser { name :: !Text, screen_name :: !Text } deriving (Generic)
instance FromJSON TwitterUser
instance ToJSON TwitterUser
instance Show TwitterUser where
  show user' = unpack (name user') ++ " (@" ++ unpack (screen_name user') ++ ")"

data Tweet = Tweet { text :: !Text, user :: !TwitterUser, entities :: !TwitterEntities } deriving (Generic)
instance FromJSON Tweet
instance ToJSON Tweet
instance Show Tweet where
  show tweet = show (user tweet) ++ ": " ++ unpack (replaceUrls (urls (entities tweet)) (text tweet))

replaceUrls :: [TwitterUrl] -> Text -> Text
replaceUrls [] t = t
replaceUrls (u:us) t = replaceUrls us (replace (url u) (expanded_url u) t)

readTweet :: String -> IRC (Either String Tweet)
readTweet id' = do
  req <- authRequest ("https://api.twitter.com/1.1/statuses/show.json?id=" ++ id')
  return $ case req of
    Left err -> Left err
    Right r -> eitherDecode r
