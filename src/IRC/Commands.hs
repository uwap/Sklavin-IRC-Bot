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

invite :: Nick -> String -> IRC ()
invite nick chan = write $ ucInvite nick chan

joinChannel :: String -> IRC ()
joinChannel chan = write $ ucJoin chan

pong :: String -> IRC ()
pong = write . ucPong

privmsg :: String -> String -> IRC ()
privmsg chan = write . ucPrivmsg chan

configuratedCommand :: Nick -> String -> [String] -> IRC ()
configuratedCommand _ _ [] = return ()
configuratedCommand (Nick nick) channel (comm:args) = do
    let name = "Commands." ++ comm
    commands <- fromMaybe [] <$> lookupGlobalConfig (name ++ ".reply")
    unless (null commands) $ do
      random <- liftIO randomIO
      let command = commands !! (random `mod` length commands)
      argsReplace <- replaceArgs name
      let reply = replaceAll (pack command) [("@nick",nick), ("@channel",channel), ("@args",argsReplace)]
      privmsg channel (unpack reply)
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
