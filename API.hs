{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}

module API
( Task
, getCurrent
, stopTask
, description
) where

import Data.Maybe
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import Control.Monad
import Control.Lens ((&), (^=), (?~), (^.))
import GHC.Generics

import Network.Wreq
import Data.Aeson

import Settings

data Task = Task
    { id           :: Maybe Int
    , wid          :: Maybe Int
    , pid          :: Maybe Int
    , guid         :: Maybe T.Text
    , duration     :: Maybe Int
    , billable     :: Maybe Bool
    , duronly      :: Maybe Bool
    , start        :: Maybe T.Text
    , stop         :: Maybe T.Text
    , description  :: Maybe T.Text
    , at           :: Maybe T.Text
    , created_with :: Maybe T.Text
    , tags         :: Maybe T.Text
    } deriving (Show, Generic)
instance FromJSON Task
instance ToJSON Task where
  toJSON t = object $ catMaybes
    [ ("id" .=)           <$> API.id t
    , ("wid" .=)          <$> wid t
    , ("pid" .=)          <$> pid t
    , ("guid" .=)         <$> guid t
    , ("duration" .=)     <$> duration t
    , ("start" .=)        <$> start t
    , ("stop" .=)         <$> stop t
    , ("description" .=)  <$> description t
    , ("at" .=)           <$> API.at t
    , ("tags" .=)         <$> tags t
    , ("created_with" .=) <$> created_with t
    ]
emptyTask = Task
  { API.id       = Nothing
  , wid          = Nothing
  , pid          = Nothing
  , guid         = Nothing
  , duration     = Nothing
  , billable     = Nothing
  , duronly      = Nothing
  , start        = Nothing
  , stop         = Nothing
  , description  = Nothing
  , API.at       = Nothing
  , tags         = Nothing
  , created_with = Nothing
  }

data Wrap1 = Wrap1
   { _data :: Task
   } deriving (Show, Generic)
instance FromJSON Wrap1 where
  parseJSON (Object v) = Wrap1 <$> v .: "data"
  parseJSON _ = mzero
parseResponse1 :: B.ByteString -> Maybe Task
parseResponse1 str = _data <$> wrap where
  wrap = decode $ str :: Maybe Wrap1

data TimeEntry = TimeEntry
  { time_entry :: Task
  } deriving (Show, Generic)
instance ToJSON TimeEntry

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

startTask :: B.ByteString -> IO (Maybe Task)
startTask desc = do
  let entry = TimeEntry { time_entry = emptyTask { description = Just $ T.decodeUtf8 desc , created_with = Just "toggl-cli" } }
  opts <- getOption
  r <- postWith opts (toggl_url ++ "time_entries/start") (toJSON entry)
  return $ decode desc

stopTask :: Task -> IO (Maybe Task)
stopTask task = do
  opts <- getOption
  r <- putWith opts (toggl_url ++ "time_entries/" ++ (show $ fromJust $ API.id task) ++ "/stop") B.empty
  return $ parseResponse1 $ r ^. responseBody
