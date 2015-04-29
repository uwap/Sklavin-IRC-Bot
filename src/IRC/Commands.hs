{-# LANGUAGE OverloadedStrings #-}
module IRC.Commands where

import IRC.Connection
import IRC.Proto
import IRC.Config

import System.Random (randomIO)

import Control.Monad.Reader (liftIO)
import Control.Monad.Trans.Reader

import Data.Text (Text, pack, unpack, replace)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

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

configuratedCommand :: Nick -> Channel -> [String] -> C.Name -> IRC ()
configuratedCommand (Nick nick) chan@(Channel channel) (comm:args) name = do
    conf <- asks config
    maybeCommand <- liftIO $ C.lookup conf $ pack (unpack name ++ ".reply")
    case maybeCommand of
      Nothing -> return ()
      Just commands -> do
        command <- liftIO randomIO >>= \r -> return $ commands !! (r `mod` length commands)
        replaceNick           nick channel args name conf (pack command)
          >>= replaceChannel  nick channel args name conf
          >>= replaceArgs     nick channel args name conf
          >>= privmsg chan . unpack
  where
    replaceNick :: String -> String -> [String] -> C.Name -> C.Config -> Text -> IRC Text
    replaceNick nick _ _ _ _ = return . replace "@nick" (pack nick)

    replaceChannel :: String -> String -> [String] -> C.Name -> C.Config -> Text -> IRC Text
    replaceChannel _ channel _ _ _ = return . replace "@channel" (pack channel)

    replaceArgs :: String -> String -> [String] -> C.Name -> C.Config -> Text -> IRC Text
    replaceArgs nick _ args' name conf text = case args' of
        [] -> do
          maybeReplace <- liftIO $ C.lookup conf $ pack (unpack name ++ ".replaceEmptyArgsWithNick")
          case maybeReplace of
            Just True -> return $ replace "@args" (pack nick) text
            _ -> return $ replace "@args" (pack $ unwords args) text
        args -> return $ replace "@args" (pack $ unwords args) text


