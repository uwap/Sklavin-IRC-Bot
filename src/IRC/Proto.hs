module IRC.Proto where

import IRC.Connection
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
    nick = Nick . takeWhile (/= '!') . drop 1
    name = Name . takeWhile (/= '@') . drop 1 . dropWhile (/= '!')
    host = Host . drop 1 . dropWhile (/= '@')

newtype Prefix   = Prefix String
newtype Command  = Command String
type    Params   = [String]

{--
 RFC2812 defines a message as
    message    =  [ ":" prefix SPACE ] command [ params ] crlf
 This parses prefix, command and params out of it.
--}
parseCommand :: String -> Maybe (Prefix, Command, Params)
parseCommand (':':_) = Nothing
parseCommand s       = Just (source s, command s, params s)
                  where
                     source   = Prefix . takeWhile (/= ' ') . drop 1
                     command  = Command . takeWhile (/= ' ') . drop 1 . dropWhile (/= ' ')
                     params   = parseParams . dropWhile (/= ' ') . drop 1 . dropWhile (/= ' ')
               
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
parseParams s = filter (not . null) $ parseMiddles s ++ (parseTrailing s : [])
            where
              parseMiddles s = let middles = filter (not . null) $ parseMiddles' s in
                        take 14 middles ++ (return . unwords) (drop 14 middles)
              parseMiddles'  = words . takeWhile (/= ':')
              parseTrailing  = drop 1 . dropWhile (/= ':')
              
{--------------------------------------------------------------}
{------------------------ User Commands -----------------------}
{--------------------------------------------------------------}

newtype Channel = Channel String

performUserCommand :: Command -> Params -> Maybe String -> IRC ()
performUserCommand (Command c) middles Nothing = write $ c ++ " " ++ unwords middles
performUserCommand (Command c) middles (Just trailing) = write $ c ++ " " ++ unwords middles ++ " :" ++ trailing

ucAway :: Maybe String -> IRC ()
ucAway message = performUserCommand (Command "AWAY") (toList message) Nothing

ucInvite :: Nick -> Channel -> IRC ()
ucInvite (Nick n) (Channel c) = performUserCommand (Command "INVITE") [n, c] Nothing
