{-# LANGUAGE TemplateHaskell #-}
module AnimatedDangerzone.Client.Config where

import Control.Lens ( makeLenses )
import Network.Socket ( PortNumber )
import System.Console.GetOpt ( OptDescr(..), ArgDescr(..), ArgOrder(..), usageInfo, getOpt )
import System.Environment ( getProgName, getArgs )
import System.Exit ( exitWith, ExitCode(..) )
import System.IO ( hPutStrLn, stderr )
import Text.Read ( readMaybe )

data Options = Options
  { _optServerName :: String
  , _optPortNumber :: PortNumber
  , _optPlayerName :: String
  } deriving (Show, Eq, Ord)

makeLenses ''Options

defaultOptions :: Options
defaultOptions = Options
  { _optServerName = "localhost"
  , _optPortNumber = 1600
  , _optPlayerName = "Player 1"
  }

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "p" ["port"]
    (ReqArg
      (\arg o ->
        case readMaybe arg of
        Nothing -> do hPutStrLn stderr ("Invalid port number: " ++ arg)
                      exitWith (ExitFailure 1)
        Just n  -> return o { _optPortNumber = toEnum n })
      "NUMBER")
    "Port to connect to on server."
  , Option "s" ["server"]
    (ReqArg
      (\arg o -> return o { _optServerName = arg })
      "SERVER NAME")
    "Server for remote connection."
  , Option "n" ["name"]
    (ReqArg
      (\arg o -> return o { _optPlayerName = arg })
      "NAME")
    "Player name."
  , Option "h" ["help"]
    (NoArg
      (\_ -> do
        prg <- getProgName
        hPutStrLn stderr (usageInfo prg options)
        exitWith ExitSuccess))
    "Show help"
  ]

handleClientArgs :: [String] -> IO (Options, [String])
handleClientArgs args = do
  let (actions, rest, _) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  return (opts, rest)

withClientArgs :: (Options -> [String] -> IO a) -> IO a
withClientArgs io = do
  args <- getArgs
  (opts,rest) <- handleClientArgs args
  io opts rest
