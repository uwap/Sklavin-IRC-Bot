module Main where

import Core.IRC.Types
import Core.IRC.Connection
import Core.IRC.Proto

import qualified Modules.Twitter.EventListener as T
import qualified Modules.Commands as C
import qualified Modules.Logging as L

main :: IO ()
main = start [eventListener, L.logMessage, T.eventListener, T.quoteEventListener, C.eventListener]

eventListener :: Message -> IRC ()
eventListener (Ping code)             = pong code
eventListener (Invite _ chan)         = joinChannel chan
eventListener _ = return ()
