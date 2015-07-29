{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Modules.Google.Search (google) where

import GHC.Generics
import Data.Text (Text, unpack)
import Data.Aeson
import Data.String.Utils
import qualified Data.ByteString.Char8 as B
import Network.HTTP.Conduit
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseDiscard)

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

google :: (MonadIO m, MonadBaseControl IO m) => String -> (String -> m ()) -> m ()
google q f = void . liftBaseDiscard forkIO $ do
  result <- liftIO newEmptyMVar
  _ <- liftIO . forkIO $ do
    req <- parseUrl "http://ajax.googleapis.com/ajax/services/search/web"
    resp <- withManager $ \m -> responseBody <$> do
      let query = setQueryString [("v", Just "1.0"), ("q", Just $ B.pack q)] req
      httpLbs query m
    putMVar result $ case eitherDecode resp of
      Left err -> show err
      Right r  -> show $ head $ results r
  f =<< (replaceFormatting <$> liftIO (readMVar result))

-- The google api returns HTML formatting. This function turns <b> into \x0002
replaceFormatting :: String -> String
replaceFormatting = flip replaceAll [("<b>","\x0002"), ("</b>","\x000F")]
  where
    replaceAll = foldl (flip (uncurry replace)) 
