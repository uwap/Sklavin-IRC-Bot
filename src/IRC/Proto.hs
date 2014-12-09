module IRC.Proto where

import Data.List (isPrefixOf)

type Nick = String
type Name = String
type Host = String

parseUserHost :: String -> (Nick, Name, Host)
parseUserHost s = (nick s, name s, host s)
  where
    nick = takeWhile (/= '!') . drop 1
    name = takeWhile (/= '@') . drop 1 . dropWhile (/= '!')
    host = drop 1 . dropWhile (/= '@')

type Prefix   = String
type Command  = String
type Params   = [String]

{--
 RFC2812 defines a message as
    message    =  [ ":" prefix SPACE ] command [ params ] crlf
 This parses prefix, command and params out of it.
--}
parseCommand :: String -> Maybe (Prefix, Command, Params)
parseCommand (':':_) = Nothing
parseCommand s       = Just (source s, command s, params s)
                  where
                     source   = takeWhile (/= ' ') . drop 1
                     command  = takeWhile (/= ' ') . drop 1 . dropWhile (/= ' ')
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
