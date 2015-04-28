module IRC.Proto where

import Data.List (isPrefixOf)
import Data.Foldable (toList)

{--------------------------------------------------------------}
{--------------------------- Parsing --------------------------}
{--------------------------------------------------------------}

newtype Nick = Nick String
newtype Name = Name String
newtype Host = Host String

parseUserHost :: String -> (Nick, Name, Host)
parseUserHost s = (nick s, name s, host s)
  where
    nick = Nick . takeWhile (/= '!')
    name = Name . takeWhile (/= '@') . drop 1 . dropWhile (/= '!')
    host = Host . drop 1 . dropWhile (/= '@')

newtype Prefix   = Prefix String
newtype Command  = Command String
type    Params   = [String]

data Message = Message { prefix  :: Maybe Prefix
                       , command :: Command
                       , params  :: Params
                       }

{--
 RFC2812 defines a message as
    message    =  [ ":" prefix SPACE ] command [ params ] crlf
 This parses prefix, command and params out of it.
--}
parseCommand :: String -> Message
parseCommand (':':s) = Message (Just $ source s) (command s) (params s)
                  where
                     source   = Prefix . takeWhile (/= ' ')
                     command  = Command . takeWhile (/= ' ') . drop 1 . dropWhile (/= ' ')
                     params   = parseParams . dropWhile (/= ' ') . drop 1 . dropWhile (/= ' ')
parseCommand s = Message Nothing (command s) (params s)
                where
                    command  = Command . takeWhile (/= ' ')
                    params   = parseParams . dropWhile (/= ' ')
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
parseParams :: String -> Params
parseParams s = filter (not . null) $ parseMiddles s ++ ([parseTrailing s])
            where
              parseMiddles s = let middles = filter (not . null) $ parseMiddles' s in
                        take 14 middles ++ (return . unwords) (drop 14 middles)
              parseMiddles'  = words . takeWhile (/= ':')
              parseTrailing  = drop 1 . dropWhile (/= ':')
              
{--------------------------------------------------------------}
{------------------------ User Commands -----------------------}
{--------------------------------------------------------------}

newtype Channel = Channel String

uc :: Command -> Params -> Maybe String -> String
uc (Command c) middles Nothing = c ++ " " ++ unwords middles
uc (Command c) middles (Just trailing) = c ++ " " ++ unwords middles ++ " :" ++ trailing

ucAway :: Maybe String -> String
ucAway message = uc (Command "AWAY") (toList message) Nothing

ucInvite :: Nick -> Channel -> String
ucInvite (Nick n) (Channel c) = uc (Command "INVITE") [n, c] Nothing

ucQuit :: Maybe String -> String
ucQuit = uc (Command "QUIT") []

ucJoin :: Channel -> String
ucJoin (Channel chan) = uc (Command "JOIN") (return chan) Nothing

ucPong :: String -> String
ucPong c = uc (Command "PONG") [] $ Just c

ucPrivmsg :: Channel -> String -> String
ucPrivmsg (Channel c) s = uc (Command "PRIVMSG") (return c) $ Just s

{--------------------------------------------------------------}
{----------------------- Server Commands ----------------------}
{--------------------------------------------------------------}

isPrivmsg :: (Prefix, Command, Params) -> Bool
isPrivmsg (_, Command c, _) = c == "PRIVMSG"

isPing :: (Prefix, Command, Params) -> Bool
isPing (_, Command c, _) = c == "PING"
