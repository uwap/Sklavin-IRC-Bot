{-# LANGUAGE OverloadedStrings #-}
module Twitter.Auth (authRequest) where

import IRC.Types
import IRC.Config
import Control.Monad.State
import Web.Authenticate.OAuth
import Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (ByteString)

twitterOAuth :: IRC (Maybe OAuth)
twitterOAuth = do
       consumerKey    <- lookupGlobalConfig "twitter.consumerKey"
       consumerSecret <- lookupGlobalConfig "twitter.consumerSecret"
       case (consumerKey, consumerSecret) of
         (Just key, Just secret) ->
            return . Just $ newOAuth { oauthServerName     = "api.twitter.com"
                                     , oauthConsumerKey    = B.pack key
                                     , oauthConsumerSecret = B.pack secret
                                     }
         _ -> do
           liftIO $ putStrLn "[ERROR] Wasn't able to load twitter.consumerKey or twitter.consumerSecret from the config"
           return Nothing

twitterCredential :: IRC (Maybe Credential)
twitterCredential = do
  accessToken  <- lookupGlobalConfig "twitter.accessToken"
  accessSecret <- lookupGlobalConfig "twitter.accessSecret"
  case (accessToken, accessSecret) of
    (Just token, Just secret) -> return $ Just (newCredential token secret)
    _ -> do
      liftIO $ putStrLn "[ERROR] Wasn't able to load twitter.accessToken or twitter.accessSecret from the config"
      return Nothing

authRequest :: String -> IRC (Either String ByteString)
authRequest url = do
  cred'  <- twitterCredential
  oauth' <- twitterOAuth
  case (cred', oauth') of
    (Just cred, Just oauth) -> do
      req   <- parseUrl url
      res   <- withManager $ \m -> do
        signed <- signOAuth oauth cred req
        httpLbs signed m
      return $ Right (responseBody res)
    _ -> return $ Left "Wasn't able to authentificate to twitter"
