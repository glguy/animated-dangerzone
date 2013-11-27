module Main where

import AnimatedDangerzone.Types
import Control.Lens
import Control.Applicative
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
  do case w^.worldPlayers.at c of
       Nothing -> do putStrLn "User disconnected: (unknown)"
                     return w
       Just p -> do
	  let w' = w & worldPlayers . at c .~ Nothing
                     & worldPastPlayers . at (p^.playerName) .~ (Just p)
          announce hs        $ QuitPlayer c
          putStrLn $ "User disconnected: " ++ p^.playerName
          return w'

myCommand :: Handles -> ConnectionId -> ClientMsg -> World -> IO World
myCommand hs c msg w =
  -- Depending on whether this connection corresponds to a known player, handle
  -- the message differently.
  let handler = case w^.worldPlayers.at c of
                  Nothing -> handleUnknownPlayerCommand
                  Just _ -> handleKnownPlayerCommand
  in handler hs c msg w

handleUnknownPlayerCommand :: Handles -> ConnectionId -> ClientMsg -> World -> IO World
handleUnknownPlayerCommand hs c msg w =
  case msg of
    ClientHello name -> do
      -- Use the player data already in the world (if previously connected) or
      -- create a new player record otherwise.
      let Just p = w^.worldPastPlayers.at name <|> Just (newPlayer name)
          w' = w & worldPlayers . at c ?~ p

      putStrLn $ "User connected: " ++ name
      putStrLn $ "  player info:  " ++ show p

      announceOne hs c $ Hello c
      announce hs      $ NewPlayer c (p^.playerName) (p^.playerCoord)
      announceOne hs c $ SetWorld w'
      return w'
    _ -> return w

handleKnownPlayerCommand :: Handles -> ConnectionId -> ClientMsg -> World -> IO World
handleKnownPlayerCommand hs c msg w =
  case msg of
    ClientMove coord -> do putStrLn $ "Player " ++ show c ++ " moved to " ++ show coord
			   announce hs $ MovePlayer c coord
                           let w' = w & worldPlayers . ix c . playerCoord .~ coord
                           return w'
    _ -> return w

initialWorld :: World
initialWorld = World
  { _worldPlayers = Map.empty
  , _worldBlocks  = Map.fromList [((r,c), Rock) | r <- [-10..10], c <- [-10..10]]
  , _worldPastPlayers = Map.empty
  }

newPlayer :: String -> Player
newPlayer name = Player
  { _playerName         = name
  , _playerCoord        = (0,0)
  }

main :: IO ()
main = serverMain callbacks initialWorld
