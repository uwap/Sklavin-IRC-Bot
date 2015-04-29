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
      random <- liftIO randomIO
      let command = commands !! (random `mod` length commands)
      argsReplace <- replaceArgs name
      let reply = replaceAll (pack command) [("@nick",nick), ("@channel",channel), ("@args",argsReplace)]
      privmsg chan (unpack reply)
  where
    replaceAll :: Text -> [(String,String)] -> Text
    replaceAll = foldl (\text (pattern,repl) -> replace (pack pattern) (pack repl) text)
    
    replaceArgs :: String -> IRC String
    replaceArgs name  = do
      setting <- return . fromMaybe False =<< lookupGlobalConfig (name ++ ".replaceEmptyArgsWithNick")
      if setting && null args then
        return nick
      else
        return (unwords args)
