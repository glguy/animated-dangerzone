{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Data.Map as M
import Network
import NetworkedGame.Packet
import NetworkedGame.Server
import Control.Concurrent
import Control.Monad
import Control.Lens
import Data.Maybe
import System.IO
import System.Exit
import AnimatedDangerzone.Types
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Game

import AnimatedDangerzone.Client.Config as C

data ClientState =
  ClientState { _clientCid :: ConnectionId
	      , _clientWorld :: World
	      , _clientHandle :: Handle
	      }

data Tile =
    TEnv Block
  | TPlayer
  deriving (Eq, Read, Show, Ord)

makeLenses ''ClientState

tileFiles :: [(Tile, FilePath)]
tileFiles =
  [ (TEnv Air,        "images/tiles/dc-dngn/dngn_unseen.png")
  , (TEnv Rubble,     "images/tiles/dc-dngn/floor/cobble_blood5.png")
  , (TEnv Rock,       "images/tiles/dc-dngn/floor/cobble_blood1.png")
  , (TEnv Stones,     "images/tiles/dc-dngn/floor/floor_vines0.png")
  , (TEnv Lava,       "images/tiles/dc-dngn/floor/lava0.png")
  , (TEnv Ice,        "images/tiles/dc-dngn/floor/ice0.png")
  , (TPlayer,         "images/tiles/player/base/human_m.png")
  ]

loadTileMap :: IO (M.Map Tile Picture)
loadTileMap = do
  pairs <- forM tileFiles $ \(t, path) ->
	   do Just p <- loadJuicy path
	      return (t, p)

  return $ M.fromList pairs

clientState :: Handle -> ConnectionId -> World -> ClientState
clientState h cid w =
  ClientState { _clientCid = cid
	      , _clientWorld = w
	      , _clientHandle = h
	      }

getPlayer :: MVar ClientState -> IO Player
getPlayer mv = do
  cs <- readMVar mv
  return $ fromJust $ cs^.clientWorld.worldPlayers.at (cs^.clientCid)

main :: IO ()
main = do
  tileMap <- loadTileMap

  withClientArgs $ \opts _ -> do
    h      <- connectTo (opts^.optServerName)
                        (PortNumber (opts^.optPortNumber))
    hPutPacket h $ mkPacket $ ClientHello (opts^.optPlayerName)
    resp <- hGetPacketed h
    myCid <- case resp of
               Hello myCid -> return myCid
               UsernameConflict -> do
                       putStrLn "User already connected; please choose a different username."
                       exitFailure
               _ -> do putStrLn "Protocol error: got unexpected message"
                       exitFailure

    NewPlayer _ _ <- hGetPacketed h
    SetWorld initialWorld <- hGetPacketed h

    wmvar <- newMVar $ clientState h myCid initialWorld
    _ <- forkIO $ forever $ networkThread h wmvar

    let dpy = InWindow "game" (700, 700) (0, 0)

    playIO
      dpy
      white
      60
      ()
      (const $ worldPicture wmvar tileMap)
      (handleEvent wmvar)
      stepWorld

moveUp :: MVar ClientState -> IO ()
moveUp mv = movePlayer mv (_1 +~ 1)

moveDown :: MVar ClientState -> IO ()
moveDown mv = movePlayer mv (_1 -~ 1)

moveLeft :: MVar ClientState -> IO ()
moveLeft mv = movePlayer mv (_2 -~ 1)

moveRight :: MVar ClientState -> IO ()
moveRight mv = movePlayer mv (_2 +~ 1)

movePlayer :: MVar ClientState -> (Coord -> Coord) -> IO ()
movePlayer mv f = do
  p <- getPlayer mv
  let old = p^.playerCoord
  sendMessage mv $ ClientMove $ f old

sendMessage :: MVar ClientState -> ClientMsg -> IO ()
sendMessage mv msg = do
  cs <- readMVar mv
  hPutPacket (cs^.clientHandle) $ mkPacket msg

handleEvent :: MVar ClientState -> Event -> () -> IO ()
handleEvent mv e _st = do
  case e of
    EventKey (SpecialKey KeyUp) Down _ _ -> moveUp mv
    EventKey (SpecialKey KeyDown) Down _ _ -> moveDown mv
    EventKey (SpecialKey KeyLeft) Down _ _ -> moveLeft mv
    EventKey (SpecialKey KeyRight) Down _ _ -> moveRight mv
    _ -> return ()
  return ()

-- Later, animation
stepWorld :: Float -> () -> IO ()
stepWorld = const return

networkThread :: Handle -> MVar ClientState -> IO ()
networkThread h mv = do
  p <- hGetPacketed h
  putStrLn $ "Got server packet: " ++ show p
  case p of
    SetWorld w -> modifyMVar_ mv $ \cs -> return $ cs & clientWorld .~ w
    MovePlayer cid coord -> modifyMVar_ mv $ \cs ->
        return $ cs & clientWorld.worldPlayers.ix cid.playerCoord .~ coord
    un -> putStrLn $ "Unsupported message: " ++ show un

worldPicture :: MVar ClientState -> M.Map Tile Picture -> IO Picture
worldPicture mv tileMap = do
  cs <- readMVar mv

  let w = cs^.clientWorld
      tilePicture (r,c) Nothing = trans (r,c) (TEnv Air)
      tilePicture (r,c) (Just b) = trans (r,c) (TEnv b)
      playerPicture p = trans (p^.playerCoord) TPlayer
      trans (r,c) t = Translate (toEnum $ c*32) (toEnum $ r*32) $ tileMap M.! t
      envTiles = [ tilePicture (r,c) (w^.worldBlocks.at(r,c))
                   | c <- [-20..20], r <- [-20..20] ]
      playerTiles = [ playerPicture p | p <- M.elems (w^.worldPlayers) ]

  return $ Pictures $ envTiles ++ playerTiles
