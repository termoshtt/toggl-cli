{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import Data.Maybe
import Control.Lens

import Settings
import API

data MyOptions =
    CurrentMode {
    } |
    StopMode {
    } deriving (Data, Typeable, Show, Eq)
 
currentMode :: MyOptions
currentMode = CurrentMode {} &= name "current"
 
stopMode :: MyOptions
stopMode = StopMode {} &= name "stop"
 
myModes :: Mode (CmdArgs MyOptions)
myModes = cmdArgsMode $ modes [currentMode, stopMode]
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME
 
_PROGRAM_NAME = "toggl-cli"
_PROGRAM_VERSION = "0.1"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "CLI-client for Toggl"
 
main :: IO ()
main = do
    args <- getArgs
    opts <- (if null args then withArgs ["--help"] else id) $ cmdArgsRun myModes
    exec opts

printTask :: Maybe Task -> IO ()
printTask task = T.putStrLn $ fromJust $ description =<< task

exec :: MyOptions -> IO ()
exec opts@CurrentMode = getCurrent >>= printTask
exec opts@StopMode = do
  mtask <- getCurrent
  case mtask of
    Just task -> stopTask task >>= printTask
    Nothing -> error "No task is running."
