module AnimatedDangerzone.Types where

import Data.Binary
import Data.Map (Map)

type Coord = (Int,Int)

data ClientMsg
  = ClientMove Coord
  | ClientHello String
  deriving (Read, Show)

data ServerMsg
  = SetWorld World (Map Int (String, Coord))
  deriving (Read, Show)

data World = World
  { worldBlocks :: Map Coord Block
  , worldPlayers :: Map Int Player
  }
  deriving (Read, Show)

data Block
  = Rock
  | Rubble
  | Air
  deriving (Read, Show)

data Player
  = Player { playerPos :: Coord }
  deriving (Read, Show)

instance Binary ClientMsg where
  put = put . show
  get = read `fmap` get

instance Binary ServerMsg where
  put = put . show
  get = read `fmap` get
