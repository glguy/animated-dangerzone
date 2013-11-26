module Main where

import AnimatedDangerzone.Types
import Control.Lens
import NetworkedGame.Server
import qualified Data.Map as Map
import Network (PortID(..))
import System.IO

callbacks :: NetworkServer ClientMsg World
callbacks = NetworkServer
  { serverPort          = PortNumber 1600
  , eventsPerSecond     = -1 -- no timer yet
  , onTick              = myTick
  , onConnect           = myConnect
  , onDisconnect        = myDisconnect
  , onCommand           = myCommand
  }

myTick :: Handles -> Float -> World -> IO World
myTick hs elapsed w = return w

myConnect :: Handles -> ConnectionId -> World -> IO World
myConnect hs con w = return w

myDisconnect :: Handles -> ConnectionId -> World -> IO World
myDisconnect hs c w =
  do announce hs        $ QuitPlayer c
     let w' = w & worldPlayers . at c .~ Nothing
	 p = w^.worldPlayers.at c
     case p of
       Nothing -> putStrLn "User disconnected: (unknown)"
       Just player -> putStrLn $ "User disconnected: " ++ player^.playerName
     return w'

myCommand :: Handles -> ConnectionId -> ClientMsg -> World -> IO World
myCommand hs c msg w =
  -- Depending on whether this connection corresponds to a known player, handle
  -- the message differently.
  case w^.worldPlayers.at c of
    Nothing -> handleUnknownPlayerCommand hs c msg w
    Just _ -> handleKnownPlayerCommand hs c msg w

handleUnknownPlayerCommand :: Handles -> ConnectionId -> ClientMsg -> World -> IO World
handleUnknownPlayerCommand hs c msg w =
  case msg of
    ClientHello name -> do
      let w' = w & worldPlayers . at c ?~ p
	  p = newPlayer name
      putStrLn $ "User connected: " ++ name
      announceOne hs c $ Hello c
      announce hs      $ NewPlayer c (p^.playerName) (p^.playerCoord)
      announceOne hs c $ SetWorld w'
      return w'
    _ -> return w

handleKnownPlayerCommand :: Handles -> ConnectionId -> ClientMsg -> World -> IO World
handleKnownPlayerCommand hs c msg w =
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

newPlayer :: String -> Player
newPlayer name = Player
  { _playerName         = name
  , _playerCoord        = (0,0)
  }

main :: IO ()
main = serverMain callbacks initialWorld
