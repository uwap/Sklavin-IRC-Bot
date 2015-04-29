{-# LANGUAGE OverloadedStrings #-}
module IRC.Commands where

import IRC.Connection
import IRC.Proto hiding (command)
import IRC.Config

import System.Random (randomIO)

import Control.Monad
import Control.Monad.Reader (liftIO)

import Data.Text (Text, pack, unpack, replace)
import Data.Maybe (fromMaybe)

away :: String -> IRC ()
away = write . ucAway . Just

back :: IRC ()
back = write $ ucAway Nothing

invite :: Nick -> Channel -> IRC ()
invite n = write . ucInvite n

joinChannel :: Channel -> IRC ()
joinChannel c = write $ ucJoin c

pong :: String -> IRC ()
pong = write . ucPong

privmsg :: Channel -> String -> IRC ()
privmsg c = write . ucPrivmsg c

configuratedCommand :: Nick -> Channel -> [String] -> IRC ()
configuratedCommand _ _ [] = return ()
configuratedCommand (Nick nick) chan@(Channel channel) (comm:args) = do
    let name = "Command." ++ comm
    commands <- fromMaybe [] <$> lookupGlobalConfig (name ++ ".reply")
    unless (null commands) $ do
      command <- liftM ((commands !!) . (`mod` length commands)) $ liftIO randomIO
      repl "@nick" nick (pack command)
        >>= repl "@channel" channel
        >>= replaceArgs name
        >>= privmsg chan . unpack
  where
    repl :: Text -> String -> Text -> IRC Text
    repl pattern replaceString = return . replace pattern (pack replaceString)
    
    replaceArgs :: String -> Text -> IRC Text
    replaceArgs name text = do
      setting <- return . fromMaybe False =<< lookupGlobalConfig (name ++ ".replaceEmptyArgsWithNick")
      if setting && null args then
        repl "@args" nick text
      else
        repl "@args" (unwords args) text
