module Main where

import IRC.Connection
import IRC.Config

main :: IO ()
main = start $ Config "irc.german-elite.net" 6667 "Sklavin"
