module Twitter.EventListener where

import IRC.Types
import IRC.Proto
import Twitter.Tweet

import Control.Monad

import qualified Data.Text as T
import Data.List
import Data.Char

eventListener :: Message -> IRC ()
eventListener (Privmsg _ message chan) = do
          let ws = words message
          forM_ ws $ \w ->
            when ("http://twitter.com/" `isPrefixOf` w || "https://twitter.com/" `isPrefixOf` w) $ do
              let suffix = dropWhile (/= '/') $ drop 1 $ dropWhile (/= '/') (drop 9 w)
              when ("/status/" `isPrefixOf` suffix) $ do
                let status_id = drop 8 suffix
                when (and (isNumber <$> status_id)) $ do
                  tweet <- readTweet status_id
                  case tweet of
                    Left err -> privmsg chan err
                    Right t  -> privmsg chan (T.unpack $ text t)
eventListener _ = return () 
