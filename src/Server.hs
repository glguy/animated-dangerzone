{-# LANGUAGE TemplateHaskell #-}
module Main where

import AnimatedDangerzone.Types
import Control.Lens
import Control.Applicative
import NetworkedGame.Server
import qualified Data.Map as Map
import Network (PortID(..))
import System.IO

data ServerState =
  ServerState { _pastPlayers :: Map.Map String Player
              , _serverWorld :: World
              }

makeLenses ''ServerState

callbacks :: NetworkServer ClientMsg ServerState
callbacks = NetworkServer
  { serverPort          = PortNumber 1600
  , eventsPerSecond     = -1 -- no timer yet
  , onTick              = myTick
  , onConnect           = myConnect
  , onDisconnect        = myDisconnect
  , onCommand           = myCommand
  }

myTick :: Handles -> Float -> ServerState -> IO ServerState
myTick hs elapsed st = return st

myConnect :: Handles -> ConnectionId -> ServerState -> IO ServerState
myConnect hs con st = return st

myDisconnect :: Handles -> ConnectionId -> ServerState -> IO ServerState
myDisconnect hs c st =
  do case st^.serverWorld.worldPlayers.at c of
       Nothing -> do putStrLn "User disconnected: (unknown)"
                     return st
       Just p -> do
          let st' = st & serverWorld . worldPlayers . at c .~ Nothing
                       & pastPlayers . at (p^.playerName) .~ (Just p)
          announce hs        $ QuitPlayer c
          putStrLn $ "User disconnected: " ++ p^.playerName
          return st'

myCommand :: Handles -> ConnectionId -> ClientMsg -> ServerState -> IO ServerState
myCommand hs c msg st =
  -- Depending on whether this connection corresponds to a known player, handle
  -- the message differently.
  let handler = case st^.serverWorld.worldPlayers.at c of
                  Nothing -> handleUnknownPlayerCommand
                  Just _ -> handleKnownPlayerCommand
  in handler hs c msg st

handleUnknownPlayerCommand :: Handles -> ConnectionId -> ClientMsg -> ServerState -> IO ServerState
handleUnknownPlayerCommand hs c msg st =
  case msg of
    ClientHello name -> do
      -- If the name corresponds to a logged-in user, send a conflict response;
      -- else log the user in.
      case any ((== name) . _playerName) $ Map.elems (st^.serverWorld.worldPlayers) of
        True -> do
          announceOne hs c UsernameConflict
          return st
        False -> do
          -- Use the player data already in the world (if previously connected) or
          -- create a new player record otherwise.
          let Just p = st^.pastPlayers.at name <|> Just (newPlayer name)
              st' = st & serverWorld . worldPlayers . at c ?~ p
    
          putStrLn $ "User connected: " ++ name
          putStrLn $ "  player info:  " ++ show p
    
          announceOne hs c $ Hello c
          announce hs      $ NewPlayer c p
          announceOne hs c $ SetWorld (st'^.serverWorld)
          return st'
    _ -> return st

handleKnownPlayerCommand :: Handles -> ConnectionId -> ClientMsg -> ServerState -> IO ServerState
handleKnownPlayerCommand hs c msg st =
  case msg of
    ClientMove coord -> do putStrLn $ "Player " ++ show c ++ " moved to " ++ show coord
			   announce hs $ MovePlayer c coord
                           let st' = st & serverWorld . worldPlayers . ix c . playerCoord .~ coord
                           return st'
    _ -> return st

initialWorld :: World
initialWorld = World
  { _worldPlayers = Map.empty
  , _worldBlocks  = Map.fromList [ ((r,c), b)
				 | (r, row) <- zip [0..] $ reverse blocks
				 , (c, b) <- zip [0..] row
				 ]
  }

blocks :: [[Block]]
blocks =
  [ [Air,   Air,   Air,   Rock,  Rock,  Rock,  Rock,  Rock]
  , [Air,   Rock,  Rock,  Air,   Air,   Air,   Air,   Air,   Rock,  Rock]
  , [Rock,  Lava,  Stones,Air,   Air,   Air,   Air,   Air,   Air,   Air,   Rock]
  , [Rock,  Lava,  Stones,Air,   Air,   Air,   Air,   Air,   Air,   Air,   Rock]
  , [Rock,  Lava,  Stones,Air,   Air,   Air,   Air,   Air,   Air,   Air,   Rock]
  , [Rock,  Stones,Air,   Air,   Air,   Air,   Air,   Air,   Air,   Air,   Rock]
  , [Rock,  Air,   Air,   Air,   Air,   Air,   Air,   Air,   Air,   Air,   Rock]
  , [Rock,  Air,   Air,   Air,   Air,   Air,   Air,   Air,   Air,   Air,   Rock]
  , [Rock,  Air,   Air,   Air,   Air,   Air,   Air,   Air,   Air,   Air,   Rock]
  , [Rock,  Air,   Air,   Air,   Air,   Air,   Air,   Ice,   Ice,   Air,   Rock]
  , [Rock,  Air,   Air,   Air,   Air,   Air,   Air,   Air,   Ice,   Air,   Rock]
  , [Rock,  Air,   Air,   Air,   Air,   Air,   Air,   Air,   Air,   Air,   Rock]
  , [Air,   Rock,  Rock,  Air,   Air,   Air,   Air,   Air,   Rock, Rock]
  , [Air,   Air,   Air,   Rock,  Rock,  Rock,  Rock,  Rock]
  ]

newPlayer :: String -> Player
newPlayer name = Player
  { _playerName         = name
  , _playerCoord        = (0,0)
  }

initialState :: ServerState
initialState = ServerState
  { _serverWorld = initialWorld
  , _pastPlayers = Map.empty
  }

main :: IO ()
main = serverMain callbacks initialState
