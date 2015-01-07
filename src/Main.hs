module Main where

import IRC.Connection
import IRC.Config
import IRC.Commands
import IRC.Proto

main :: IO ()
main = start $ Config "irc.german-elite.net" 6667 "Sklavin" eventListener

eventListener :: Message -> IRC ()
eventListener (Message _ (Command "PING") s)         = pong (head s)
eventListener (Message _ (Command "INVITE") chan)    = mapM_ (joinChannel . Channel) $ tail chan
eventListener (Message p (Command "PRIVMSG") params) = do
            case p of
              Nothing              -> return ()
              Just (Prefix source) -> do
                let (Nick nick, _, _) = parseUserHost source
                case last params of
                  ('!':'h':'i':_) -> privmsg (Channel $ head params) $ "Hey " ++ nick
                  _               -> return ()
                                                
eventListener (Message _ _ _)                        = return ()
