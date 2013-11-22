module Client where

import AnimatedDangerzone.Types
import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Graphics.Vty
import Network
import NetworkedGame.Packet
import System.Environment
import System.Exit

data GameEvent
  = VtyEvent Event
  | NetEvent ServerMsg

main = do
  [host] <- getArgs
  h      <- connectTo host (PortNumber 1600)
  Hello myCid     <- hGetPacketed h
  NewPlayer _ _ _ <- hGetPacketed h
  SetWorld w      <- hGetPacketed h

  events <- newChan

  forkIO $ forever $ networkThread events h
  forkIO $ forever $ userThread events

  bracket mkVty shutdown $ \vty ->
    let loop w = do update vty (pic_for_image (worldImage w))
                    ev <- readChan events
                    case ev of
                      VtyEvent (EvKey KEsc _) -> exitSuccess
                      _ -> loop w
    in loop w

networkThread events h = return ()

userThread events = return ()

worldImage :: World -> Image
worldImage w = vert_cat [
               horiz_cat [
                 cellImage (w^.worldBlocks.at(r,c))
                  | c <- [-20..20] ]
                  | r <- [-20..20] ]

cellImage Nothing = char def_attr '.'
cellImage (Just Rock) = char def_attr 'R'
cellImage (Just Rubble) = char def_attr 'r'
cellImage (Just Air) = char def_attr '_'
