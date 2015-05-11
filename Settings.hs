{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Settings
( getApiToken
) where

import qualified Data.ByteString.Lazy as B
import Data.Yaml
import System.Directory
import GHC.Generics

data Setting = Setting
   { api_token  :: String
   } deriving (Show, Generic)

instance FromJSON Setting

rcFilename :: IO String
rcFilename = do
  home <- getHomeDirectory
  return $ home ++ "/.togglcli.yaml"

getSetting :: IO Setting
getSetting = do
  fn <- rcFilename
  buf <- B.readFile fn
  case (decode $ B.toStrict buf :: Maybe Setting) of
    Just st -> return st
    Nothing -> error "Cannot read setting file"

getApiToken :: IO String
getApiToken = do
  setting <- getSetting
  return $ api_token setting

main :: IO ()
main = do
  setting <- getSetting
  print setting
