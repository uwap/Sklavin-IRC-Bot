{-# LANGUAGE OverloadedStrings #-}
module Modules.Twitter.EventListener 
  ( eventListener
  , quoteEventListener
  ) where

import Core.IRC.Types
import Core.IRC.Proto
import Core.IRC.Util
import Modules.Twitter.Auth
import Modules.Twitter.Tweet hiding (name)
import Network.HTTP.Conduit

import Control.Lens
import Control.Monad
import Control.Monad.Catch (handle, SomeException)
import Control.Monad.State (liftIO)

import Data.List
import Data.Char
import qualified Data.List.Utils as U
import Data.ByteString.Char8 (pack)

handleError :: Channel -> SomeException -> IRC ()
handleError chan exc = do
  liftIO $ print exc
  privmsg chan "An error occured. Maybe you tried reading on a private account or writing something invalid?"

eventListener :: Message -> IRC ()
eventListener (Privmsg _ message chan) = eventHandler message chan
eventListener _ = return ()

containingStatusIDs :: String -> [String]
containingStatusIDs message = 
    let ws = words message in join $ ws & each %~ \url ->
      guard ("http://twitter.com/" `isPrefixOf` url || "https://twitter.com/" `isPrefixOf` url) *> do
        let suffix = dropWhile (/= '/') $ drop 1 $ dropWhile (/= '/') (drop 9 url)
        let status_id = drop 1 . dropWhile (/= '/') . drop 1 $ suffix
        guard (("/status/" `isPrefixOf` suffix
            || "/statuses/" `isPrefixOf` suffix)
            && and (isNumber <$> status_id)) *>
          return status_id

eventHandler :: String  -> Channel -> IRC ()
eventHandler message chan = forkIRC_ . handle (handleError chan) $
    sequence_ $ containingStatusIDs message & each %~ \status_id -> do
      tweet <- readTweet status_id
      case tweet of
        Left err -> privmsg chan err
        Right t  -> do
          privmsg chan (printBox . lines $ show t)
          eventHandler (show t) chan

quoteEventListener :: Message -> IRC ()
quoteEventListener (Privmsg user' message chan) = forkIRC_ . handle (handleError chan) $
    -- Ewwww. Fixed channel and user? Change it!
    when ("!tweet " `isPrefixOf` message && ((chan ^. name) == "#aspies" || user' == "uwap")) $ do
      let tweet = drop 1 (dropWhile (/= ' ') message) ++ "\n\n~" ++ user'
      let requestBody' = urlEncodedBody [("status", lineUp tweet)]
      result <- authRequest "https://api.twitter.com/1.1/statuses/update.json" requestBody'
      case result of
        Left err -> privmsg chan err
        Right _  -> privmsg chan "Tweeted!"
  where
    lineUp = pack . U.join "\n<" . U.split "<"
quoteEventListener _ = return ()

printBox :: [String] -> String
printBox (user:message) =
    "+" ++ replicate 72 '-' ++ "+\n" ++
    "+ \x0002" ++ take 70 user ++ replicate (70 - length user) ' ' ++ "\x000F +\n"
    ++ printlines message ++
    "+" ++ replicate 72 '-' ++ "+\n"
  where
    printlines [] = ""
    printlines (x:xs) = do
      let (t,r) = if length x > 70 then splitLastWord x else (x,"")
      let prep = guard (not . and $ isSpace <$> t) *> "+ " ++ t ++ replicate (70 - length t) ' ' ++ " +\n"
      prep ++ (if and (isSpace <$> r) then printlines xs
                  else printlines (r:xs))
                          
    splitLastWord str = let rvsd = reverse $ take 70 str in
                          ( reverse (dropWhile (not . isSpace) rvsd)
                          , reverse (takeWhile (not . isSpace) rvsd) ++ drop 70 str
                          )
