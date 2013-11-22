module Main where

import AnimatedDangerzone.Types
import Control.Lens
import NetworkedGame.Server
import qualified Data.Map as Map
import Network (PortID(..))

callbacks :: NetworkServer ClientMsg World
callbacks = NetworkServer
  { serverPort          = PortNumber 1600
  , eventsPerSecond     = 0 -- no timer yet
  , onTick              = myTick
  , onConnect           = myConnect
  , onDisconnect        = myDisconnect
  , onCommand           = myCommand
  }

myTick :: Handles -> Float -> World -> IO World
myTick hs elapsed w = return w

myConnect :: Handles -> ConnectionId -> World -> IO World
myConnect hs con w =
  do let w' = w & worldPlayers . at con ?~ emptyPlayer
     announceOne hs con $ Hello con
     announce hs        $ NewPlayer con "Unknown" (0,0) -- XXX: work out names
     announceOne hs con $ SetWorld w'
     return w'

myDisconnect :: Handles -> ConnectionId -> World -> IO World
myDisconnect hs c w =
  do announce hs        $ QuitPlayer c
     let w' = w & worldPlayers . at c .~ Nothing
     return w'

myCommand :: Handles -> ConnectionId -> ClientMsg -> World -> IO World
myCommand hs c msg w =
  case msg of
    ClientMove coord -> do announce hs $ MovePlayer c coord
                           let w' = w & worldPlayers . ix c . playerCoord .~ coord
                           return w'
    _ -> return w

initialWorld :: World
initialWorld = World
  { _worldPlayers = Map.empty
  , _worldBlocks  = Map.fromList [((r,c), Rock) | r <- [-10..10], c <- [-10..10]]
  }

emptyPlayer :: Player
emptyPlayer = Player
  { _playerName         = "Unknown player"
  , _playerCoord        = (0,0)
  }

main :: IO ()
main = serverMain callbacks initialWorld
