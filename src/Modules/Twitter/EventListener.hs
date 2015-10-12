{-# LANGUAGE OverloadedStrings #-}
module Modules.Twitter.EventListener 
  ( eventListener
  , quoteEventListener
  ) where

import Core.IRC.Types
import Core.IRC.Proto
import Modules.Twitter.Auth
import Modules.Twitter.Tweet hiding (name)

import Network.HTTP.Conduit

import Control.Lens
import Control.Concurrent
import Control.Monad
import Control.Monad.Catch (handle, SomeException)
import Control.Monad.State (liftIO)
import Control.Monad.Trans.Control (liftBaseDiscard)

import Data.List
import Data.Char
import Data.List.Utils as U
import Data.ByteString.Char8 (pack)

handleError :: Channel -> SomeException -> IRC ()
handleError chan exc = do
  liftIO $ print exc
  privmsg chan "An error occured. Maybe you tried reading on a private account or writing something invalid?"

eventListener :: Message -> IRC ()
eventListener (Privmsg _ message chan) = void $ liftBaseDiscard forkIO $
        handle (handleError chan) $ do
          let ws = words message
          forM_ ws $ \w ->
            when ("http://twitter.com/" `isPrefixOf` w || "https://twitter.com/" `isPrefixOf` w) $ do
              let suffix = dropWhile (/= '/') $ drop 1 $ dropWhile (/= '/') (drop 9 w)
              when ("/status/" `isPrefixOf` suffix) $ do
                let status_id = drop 8 suffix
                when (and (isNumber <$> status_id)) $ do
                  tweet <- readTweet status_id
                  case tweet of
                    Left err -> privmsg chan err
                    Right t  -> privmsg chan (show t)
eventListener _ = return ()

quoteEventListener :: Message -> IRC ()
quoteEventListener (Privmsg user' message chan) = void $ liftBaseDiscard forkIO $ handle (handleError chan) $
    -- Ewwww. Fixed channel and user? Change it!
    when ("!tweet " `isPrefixOf` message && ((chan ^. name) == "#aspies" || user' == "uwap")) $ do
      let tweet = drop 1 (dropWhile (/= ' ') message) ++ "\n\n~" ++ user'
      let requestBody' = urlEncodedBody [("status", lineUp tweet)]
      result <- authRequest "https://api.twitter.com/1.1/statuses/update.json" requestBody'
      case result of
        Left err -> privmsg chan err
        Right _  -> privmsg chan "Tweeted!"
  where
    lineUp = pack . U.join "\n<" . split "<"
quoteEventListener _ = return ()
