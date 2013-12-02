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
  { _serverName :: String
  , _portNumber :: PortNumber
  , _playerName :: String
  } deriving (Show, Eq, Ord)

makeLenses ''Options

defaultOptions :: Options
defaultOptions = Options
  { _serverName = "localhost"
  , _portNumber = 1600
  , _playerName = "Player 1"
  }

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "p" ["port"]
    (ReqArg
      (\arg o ->
        case readMaybe arg of
        Nothing -> do hPutStrLn stderr ("Invalid port number: " ++ arg)
                      exitWith (ExitFailure 1)
        Just n  -> return o { _portNumber = toEnum n })
      "NUMBER")
    "Port to connect to on server."
  , Option "s" ["server"]
    (ReqArg
      (\arg o -> return o { _serverName = arg })
      "SERVER NAME")
    "Server for remote connection."
  , Option "n" ["name"]
    (ReqArg
      (\arg o -> return o { _playerName = arg })
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

handleArgs :: [String] -> IO (Options, [String])
handleArgs args = do
  let (actions, rest, errs) = getOpt RequireOrder options args
  case errs of
    [] -> do
      opts <- foldl (>>=) (return defaultOptions) actions
      return (opts, rest)
    _  -> do
      prg <- getProgName
      hPutStrLn stderr (usageInfo prg options)
      mapM_ (hPutStrLn stderr) errs
      exitWith (ExitFailure 1)

withArgs :: (Options -> [String] -> IO a) -> IO a
withArgs io = do
  args        <- getArgs
  (opts,rest) <- handleArgs args
  io opts rest

