module IRC.Channel (
  fromName,
  updateChannels
) where

import IRC.Types
import Control.Lens
import qualified Data.Map as M

fromName :: ChannelName -> IRC Channel
fromName name' = do
  map' <- use channels
  case M.lookup name' map' of
    Just chan -> return chan
    Nothing   -> createChannel name'

createChannel :: ChannelName -> IRC Channel
createChannel name' = do
  map' <- use channels
  let chan = mkChannel name'
  channels .= M.insert name' chan map'
  return chan

mkChannel :: ChannelName -> Channel
mkChannel name' = Channel name' M.empty

getAllChannels :: IRC [Channel]
getAllChannels = uses channels M.elems

updateChannels :: Message -> IRC ()
updateChannels (Quit user _)          = mapM_ (removeUser user) =<< getAllChannels
updateChannels (Part user _ chan)     = removeUser user chan
updateChannels (Join user chan)       = addUser user chan
updateChannels _                      = return ()

addUser :: User -> Channel -> IRC ()
addUser user chan =
  channels %= M.update (\chan' -> Just (chan' & users %~ M.insert user user)) (chan ^. name)

removeUser :: User -> Channel -> IRC ()
removeUser user chan =
  channels %= M.update (\chan' -> Just (chan' & users %~ M.delete user)) (chan ^. name)
