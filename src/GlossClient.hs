module Main where

import qualified Data.Map as M
import Network
import NetworkedGame.Packet
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Lens
import System.Environment
import System.Exit
import System.IO
import AnimatedDangerzone.Types
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Picture

tileFiles :: [(Block, FilePath)]
tileFiles =
  [ (Air, "images/tiles/dc-dngn/dngn_unseen.png")
  , (Rubble, "images/tiles/dc-dngn/floor/cobble_blood5.png")
  , (Rock, "images/tiles/dc-dngn/floor/cobble_blood1.png")
  ]

loadTileMap :: IO (M.Map Block Picture)
loadTileMap = do
  pairs <- forM tileFiles $ \(blk, path) ->
	   do Just p <- loadJuicy path
	      return (blk, p)

  return $ M.fromList pairs

main :: IO ()
main = do
  tileMap <- loadTileMap

  [host] <- getArgs
  h      <- connectTo host (PortNumber 1600)
  Hello myCid     <- hGetPacketed h
  -- XXX: race condition here?
  NewPlayer _ _ _ <- hGetPacketed h
  SetWorld initialWorld <- hGetPacketed h

  wmvar <- newMVar initialWorld
  forkIO $ forever $ networkThread h wmvar

  let eventsPerSecond = 1
      dpy = FullScreen (1024, 768)

  playIO
    dpy
    white
    eventsPerSecond
    initialWorld
    (return . worldPicture tileMap)
    handleEvent
    stepWorld

handleEvent :: Event -> World -> IO World
handleEvent e w = do
  putStrLn $ "Got gloss event: " ++ show e
  return w

stepWorld :: Float -> World -> IO World
stepWorld = const return

networkThread :: Handle -> MVar World -> IO ()
networkThread h worldMVar = do
  p <- hGetPacketed h
  putStrLn $ "Got server packet: " ++ show p
  case p of
    SetWorld w -> putMVar worldMVar w
    Hello cid -> return ()
    QuitPlayer cid -> return ()
    NewPlayer cid s coord -> return ()
    MovePlayer cid coord -> return ()

worldPicture :: M.Map Block Picture -> World -> Picture
worldPicture tileMap w =
  Pictures [ tilePicture (r,c) (w^.worldBlocks.at(r,c))
             | c <- [-20..20], r <- [-20..20] ]
  where
    tilePicture (r,c) Nothing = trans (r,c) Air
    tilePicture (r,c) (Just b) = trans (r,c) b
    trans (r,c) b = Translate (toEnum $ c*32) (toEnum $ r*32) $ tileMap M.! b
