module Main where

import IRC.Types
import IRC.Connection
import IRC.Commands
import IRC.Proto
import IRC.Logging

import qualified Twitter.EventListener as T

import Control.Monad

import Data.List

main :: IO ()
main = start [eventListener, logMessage, T.eventListener]

eventListener :: Message -> IRC ()
eventListener (Ping code)             = pong code
eventListener (Invite _ chan)         = joinChannel chan
eventListener (Privmsg user msg chan) = when ("!" `isPrefixOf` msg) $ configuratedCommand user chan (words $ drop 1 msg)
eventListener _ = return ()
