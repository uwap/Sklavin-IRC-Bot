{-# LANGUAGE OverloadedStrings #-}
module IRC.Commands where

import IRC.Types
import IRC.Connection
import IRC.Config
import qualified IRC.Proto as P

import System.Random (randomIO)

import Control.Monad
import Control.Monad.Reader (liftIO)
import Control.Monad.Trans.Reader
import Control.Concurrent.Timer (oneShotTimer)
import Control.Concurrent.Suspend.Lifted

import Data.Text (Text, pack, unpack, replace)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)
import Data.Int

import Text.Read

away :: String -> IRC ()
away = write . P.evaluateUserCommand . P.away . Just

back :: IRC ()
back = write . P.evaluateUserCommand $ P.away Nothing

invite :: String -> String -> IRC ()
invite nick chan = write . P.evaluateUserCommand $ P.invite nick chan

joinChannel :: String -> IRC ()
joinChannel chan = write . P.evaluateUserCommand $ P.join chan

pong :: String -> IRC ()
pong = write . P.evaluateUserCommand . P.pong

privmsg :: String -> String -> IRC ()
privmsg chan = write . P.evaluateUserCommand . P.privmsg chan

act :: String -> String -> IRC ()
act chan = write . P.evaluateUserCommand . P.act chan

configuratedCommand :: String -> String -> [String] -> IRC ()
configuratedCommand _ _ [] = return ()
configuratedCommand nick channel (comm:args) = do
    let name = "Commands." ++ comm
    commands <- fromMaybe [] <$> lookupGlobalConfig (name ++ ".reply")
    unless (null commands) $ do
      random <- liftIO randomIO
      let command = commands !! (random `mod` length commands)
      argsReplace <- replaceArgs name
      let reply = replaceAll (pack command) [("@nick@",nick), ("@channel@",channel), ("@args@",argsReplace)]
      mapM_ executeLine $ lines reply
  where
    executeLine :: String -> IRC ()
    executeLine line | "/me" `isPrefixOf` line    = act channel $ drop 4 line
                     | "/delay" `isPrefixOf` line = executeDelay line
                     | otherwise                  = privmsg channel line
    executeDelay :: String -> IRC ()
    executeDelay line = do
              let (time:reply) = drop 1 $ words line
              let delay = readMaybe time :: Maybe Int64
              case delay of
                Nothing -> privmsg channel $ time ++ " is not a valid time"
                Just seconds -> delayReply (sDelay seconds) $ privmsg channel (unwords reply)

    replaceAll :: Text -> [(String,String)] -> String
    replaceAll command = unpack . foldl (\text (pattern,repl) -> replace (pack pattern) (pack repl) text) command
    
    replaceArgs :: String -> IRC String
    replaceArgs name  = do
      setting <- return . fromMaybe False =<< lookupGlobalConfig (name ++ ".replaceEmptyArgsWithNick")
      if setting && null args then
        return nick
      else
        return (unwords args)

delayReply :: Delay -> IRC () -> IRC ()
delayReply delay reply = do
  handle <- ask
  _ <- liftIO $ flip oneShotTimer delay $ runReaderT reply handle
  return ()
