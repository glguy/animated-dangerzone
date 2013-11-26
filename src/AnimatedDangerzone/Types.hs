{-# LANGUAGE TemplateHaskell #-}
module AnimatedDangerzone.Types where

import Data.Binary
import Data.Map (Map)
import Control.Lens
import NetworkedGame.Handles

type Coord = (Int,Int)

data ClientMsg
  = ClientMove Coord
  | ClientHello String
  deriving (Read, Show)

data ServerMsg
  = SetWorld World
  | Hello ConnectionId
  | QuitPlayer ConnectionId
  | NewPlayer ConnectionId String Coord
  | MovePlayer ConnectionId Coord
  deriving (Read, Show)

data World = World
  { _worldBlocks :: Map Coord Block
  , _worldPlayers :: Map ConnectionId Player
  , _worldPastPlayers :: Map String Player
  }
  deriving (Read, Show)

data Block
  = Rock
  | Rubble
  | Air
  deriving (Read, Show, Ord, Eq)

data Player = Player
  { _playerName :: String
  , _playerCoord :: Coord
  }
  deriving (Read, Show)

instance Binary ClientMsg where
  put = put . show
  get = read `fmap` get

instance Binary ServerMsg where
  put = put . show
  get = read `fmap` get

makeLenses ''Player
makeLenses ''World
