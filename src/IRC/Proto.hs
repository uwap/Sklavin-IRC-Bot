module IRC.Proto where

import IRC.Types
import Data.Foldable (toList)

{--------------------------------------------------------------}
{--------------------------- Parsing --------------------------}
{--------------------------------------------------------------}

parseUserHost :: String -> (String, String, String) -- Nick, Name, Host
parseUserHost s = (nick s, name s, host s)
  where
    nick = takeWhile (/= '!')
    name = takeWhile (/= '@') . drop 1 . dropWhile (/= '!')
    host = drop 1 . dropWhile (/= '@')

{--
 RFC2812 defines a message as
    message    =  [ ":" prefix SPACE ] command [ params ] crlf
 This parses prefix, command and params out of it.
--}
parseCommand :: String -> RawMessage
parseCommand (':':s) = RawMessage (Just $ source s) (cmd s) (parameters s)
                  where
                     source     = takeWhile (/= ' ')
                     cmd        = takeWhile (/= ' ') . drop 1 . dropWhile (/= ' ')
                     parameters = parseParams . dropWhile (/= ' ') . drop 1 . dropWhile (/= ' ')
parseCommand s = RawMessage Nothing (cmd s) (parameters s)
                where
                    cmd        = takeWhile (/= ' ')
                    parameters = parseParams . dropWhile (/= ' ')
{--
 Parsing Params is more complicated then parsing command or source.
 Therefor we add an extra parsing function.
 There are various cases to handle:
 1. There aren't any parameters
 2. There are up to 14 parameters
 3. There are up to 14 parameters and then a trailing parameter
 4. There is just a trailing parameter
 5. There are 14 parameters and then a trailing paramter
 6. There are 14 parameters and a trailing parameter without a colon
 
 In RFC2812 params are described as follows:
    params     =  *14( SPACE middle ) [ SPACE ":" trailing ]
               =/ 14( SPACE middle ) [ SPACE [ ":" ] trailing ]
               
 The simplest way is to parse middles and trailing seperatively.
 If there are > 14 middles, we join them.
--}
parseParams :: String -> [String]
parseParams s = filter (not . null) $ parseMiddles ++ [parseTrailing s]
            where
              parseMiddles   = let middles = filter (not . null) $ parseMiddles' s in
                        take 14 middles ++ (return . unwords) (drop 14 middles)
              parseMiddles'  = words . takeWhile (/= ':')
              parseTrailing  = drop 1 . dropWhile (/= ':')
              
{--------------------------------------------------------------}
{------------------------ User Commands -----------------------}
{--------------------------------------------------------------}
evaluateUserCommand :: UserCommand -> String
evaluateUserCommand (UserCommand cmd mids Nothing) = cmd ++ " " ++ unwords mids
evaluateUserCommand (UserCommand cmd mids (Just trail)) = cmd ++ " " ++ unwords mids ++ " :" ++ trail

away :: Maybe String -> UserCommand
away message = UserCommand { ucommand = "AWAY"
                           , middles  = toList message
                           , trailing = Nothing
                           }

invite :: String -> String -> UserCommand
invite nick channel = UserCommand { ucommand = "INVITE"
                                  , middles  = [nick, channel]
                                  , trailing = Nothing
                                  }

quit :: Maybe String -> UserCommand
quit = UserCommand "QUIT" []

join :: String -> UserCommand
join channel = UserCommand { ucommand = "JOIN"
                           , middles  = pure channel
                           , trailing = Nothing
                           }

pong :: String -> UserCommand
pong code = UserCommand { ucommand = "PONG"
                        , middles  = []
                        , trailing = Just code
                        }

privmsg :: String -> String -> UserCommand
privmsg channel msg = UserCommand { ucommand = "PRIVMSG"
                                  , middles  = pure channel
                                  , trailing = Just msg
                                  }
act :: String -> String -> UserCommand
act channel msg = privmsg channel ('\x0001' : "ACTION " ++ msg ++ "\x0001")
