{-# LANGUAGE TypeOperators #-}
module Main where

import IRC.Types
import IRC.Connection
import IRC.Commands
import IRC.Proto
import IRC.Logging

main :: IO ()
main = start eventListener

eventListener :: RawMessage -> IRC ()
eventListener (RawMessage _ "PING" s)         = pong (head s)
eventListener (RawMessage _ "INVITE" chan)    = mapM_ join $ tail chan
eventListener msg@(RawMessage _ "PRIVMSG" _)  = logMessage msg >> onPrivmsg msg
eventListener msg                             = logMessage msg

onPrivmsg :: RawMessage -> IRC ()
onPrivmsg (RawMessage (Just source) _ (channel:message)) = do
      let (n, _, _) = parseUserHost source
      case unwords message of
        ('!':xs) -> onCommand n channel $ words xs
        _        -> return ()
onPrivmsg _ = return ()

onCommand :: String -> String -> [String] -> IRC ()
onCommand _ _ [] = return ()
onCommand nick channel commands = configuratedCommand nick channel commands
