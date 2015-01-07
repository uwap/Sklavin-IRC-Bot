module Main where

import IRC.Connection
import IRC.Config
import IRC.Commands
import IRC.Proto

main :: IO ()
main = start $ Config "irc.german-elite.net" 6667 "Sklavin" eventListener

eventListener :: Message -> IRC ()
eventListener (Message _ (Command "PING") s)      = pong (head s)
eventListener (Message _ (Command "INVITE") chan) = mapM_ (joinChannel . Channel) chan
eventListener (Message _ _ _)                     = return ()
