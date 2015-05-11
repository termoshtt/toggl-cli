{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}

module API
( Task
, getCurrent
, stopTask
) where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import Control.Monad
import Control.Lens
import GHC.Generics

import Network.Wreq
import Data.Aeson
import Data.Aeson.Lens

import Settings

data Task = Task
    { id          :: Maybe Int
    , wid         :: Maybe Int
    , pid         :: Maybe Int
    , guid        :: Maybe T.Text
    , duration    :: Maybe Int
    , billable    :: Maybe Bool
    , duronly     :: Maybe Bool
    , start       :: Maybe T.Text
    , stop        :: Maybe T.Text
    , description :: Maybe T.Text
    , at          :: Maybe T.Text
    , tags        :: Maybe T.Text
    } deriving (Show, Generic)

instance FromJSON Task
instance ToJSON Task

data Wrap1 = Wrap1
   { _data :: Task
   } deriving (Show, Generic)

instance FromJSON Wrap1 where
  parseJSON (Object v) = Wrap1 <$> v .: "data"
  parseJSON _ = mzero

parseResponse1 :: B.ByteString -> Maybe Task
parseResponse1 str = _data <$> wrap where
  wrap = decode $ str :: Maybe Wrap1

toggl_url :: String
toggl_url = "https://www.toggl.com/api/v8/"

getOption :: IO Options
getOption = do
  token <- getApiToken
  return $ defaults & auth ?~ basicAuth (S.pack token) "api_token"

getCurrent :: IO (Maybe Task)
getCurrent = do
  opts <- getOption
  r <- getWith opts $ toggl_url ++ "time_entries/current"
  return $ parseResponse1 $ r ^. responseBody

stopTask :: Int -> IO (Maybe Task)
stopTask id = do
  opts <- getOption
  r <- putWith opts (toggl_url ++ "time_entries/" ++ (show id) ++ "/stop") B.empty
  return $ parseResponse1 $ r ^. responseBody

main = do
  cur <- getCurrent
  print cur
  -- case (cur :: Maybe Task) of
  --   Just task -> case (API.id task :: Maybe Int) of
  --                  Just id -> stopTask id
  --                  Nothing -> error "no ID"
  --   Nothing -> error "no Task"

