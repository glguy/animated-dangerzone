module Main where

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
import System.IO

data GameEvent
  = VtyEvent Event
  | NetEvent ServerMsg

main = do
  [host, name] <- getArgs
  h      <- connectTo host (PortNumber 1600)
  hPutPacket h $ mkPacket $ ClientHello name
  hPutStrLn stderr "a"
  Hello myCid     <- hGetPacketed h
  hPutStrLn stderr "b"
  NewPlayer _ _ _ <- hGetPacketed h
  hPutStrLn stderr "c"
  SetWorld w      <- hGetPacketed h
  hPutStrLn stderr "d"

  events <- newChan

  forkIO $ forever $ networkThread events h

  bracket mkVty shutdown $ \vty ->
    let loop w = do update vty (pic_for_image (worldImage w))
                    ev <- readChan events
                    case ev of
                      VtyEvent (EvKey KEsc _) -> exitSuccess
                      NetEvent (SetWorld w1) -> loop w1
                      _ -> loop w
    in do forkIO $ forever $ vtyThread vty events
          loop w

networkThread events h = do p <- hGetPacketed h
                            writeChan events (NetEvent p)

vtyThread vty events = do e <- next_event vty
                          writeChan events (VtyEvent e)

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
