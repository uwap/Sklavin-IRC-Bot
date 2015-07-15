{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Google.Search (google) where

import GHC.Generics
import Data.Text (Text, unpack)
import Data.Aeson
import Network.HTTP.Conduit
import Control.Monad.IO.Class

data SearchResult = SearchResult { title :: !Text
                                 , url   :: !Text
                                 } deriving (Generic)
instance Show SearchResult where
  show res = unpack (title res) ++ " " ++ unpack (url res)
instance FromJSON SearchResult
instance ToJSON SearchResult

data SearchRequest = SearchRequest { results :: ![SearchResult] } deriving (Generic, Show)
instance FromJSON SearchRequest where
  parseJSON (Object r) = do
    res <- parseJSON =<< (r .: "responseData" >>= (.: "results"))
    return $ SearchRequest res
  parseJSON _ = return $ SearchRequest []
instance ToJSON SearchRequest

google :: MonadIO m => String -> m String
google q = liftIO $ do
  let url' = "http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=" ++ q
  resp <- simpleHttp url'
  case eitherDecode resp of
    Left err -> return (show err)
    Right r  -> return (show $ head $ results r)
