{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import GHC.Generics
import Network.Http.Client
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Aeson hiding (Options)
import Data.Foldable
import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as T--(toUpper, toLower)
import OpenSSL
import Options.Generic
import System.ProgressBar

addr = "logs.tf"

data Options = Options {
  steamID64 :: Int
, steamID3 :: Text
, count :: Maybe Int
} deriving (Generic, ParseRecord, Show)

data MatchInfo = MatchInfo {
  length :: Int
, players :: [PlayerStats]
} deriving (Show, Generic, FromJSON)

data PlayerStats = PlayerStats {
  steamID :: Text
, classStats :: [ClassStats]
, kills :: Int
, deaths :: Int
} deriving (Show)

data TFClass = Scout | Soldier | Pyro | Demoman | Heavy | Engineer | Medic | Sniper | Spy
  deriving (Read, Show)

instance FromJSON TFClass where
  -- capitalize first letter from the JSON string value 
  parseJSON = withText "TFClass" $ \x ->
    let s = (T.toUpper $ T.take 1 x) <> T.drop 1 x
    in
      return $ read $ T.unpack s

data ClassStats = ClassStats {
  _type :: TFClass -- keyword as JSON field :(
, kills :: Int
, deaths :: Int
} deriving (Generic, Show)

{-instance FromJSON ClassStats where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 1 } -}

instance FromJSON ClassStats where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = fieldLabel } where
      fieldLabel "_type" = "type"
      fieldLabel s = s

{-data BaseClassStats = BaseClassStats {
  kills :: Int
, deaths :: Int
} deriving (FromJSON, Generic, Show)
-}

data LogQuery = LogQuery {
  success :: Bool
, results :: Int
, total :: Int -- Total logs for this player
, logs :: [PlayerLog]
} deriving (Show, Generic, FromJSON)

data PlayerLog = PlayerLog {
  id :: Int
} deriving (Show, Generic, FromJSON)

instance {-# OVERLAPPING #-} FromJSON [PlayerStats] where
  parseJSON x = parseJSON x >>= mapM parseEntry . HML.toList where
    parseEntry (t, v) = withObject "[PlayerStats]" (\o ->
      PlayerStats t <$>
        o .: "class_stats" <*>
        o .: "kills" <*>
        o .: "deaths") v
      --v is the JSON value containing the player objects with SteamID fields

playerLogs :: Connection -> ByteString -> IO LogQuery
playerLogs conn steamID = do
  let path = "/api/v1/log?player=" <> steamID
  let req = buildRequest1 $ (do
      http GET path
      setAccept "application/json")
  
  sendRequest conn req emptyBody
  receiveResponse conn jsonHandler :: IO LogQuery

-- Get log (MatchInfo) by id
matchInfo :: Connection -> ByteString -> IO MatchInfo
matchInfo conn id = do
  let req = buildRequest1 $ do
      http GET $ "/json/" <> id
      setAccept "application/json"
  threadDelay 100000 --Delay since logs.tf doesn't seem to like the frequent requests
  sendRequest conn req emptyBody
  receiveResponse conn jsonHandler :: IO MatchInfo

toByteString :: Show a => a -> ByteString
toByteString x = BS.pack $ show x

main = withOpenSSL $ do
  opts <- getRecord "logstf-stats" :: IO Options

  ctx <- baselineContextSSL
  conn <- openConnectionSSL ctx addr 443
  
  logQuery <- playerLogs conn $ toByteString $ steamID64 opts
  let ids = fmap Main.id $ logs $ logQuery 
  let binIDs = fmap toByteString ids
  let matchInfosM = fmap (matchInfo conn) binIDs
  
  let logCount = fromMaybe 20 $ count opts
  let logs10 = take logCount matchInfosM
  
  print $ "Fetching " <> show logCount <> " logs..."
  progBar <- newProgressBar defStyle 10 $ Progress 0 logCount ()
  stats <- forM logs10 (\log -> do
    incProgress progBar 1
    log) 

  let playerStats = join $ fmap players stats
  let onePlayerStats = filter ((steamID3 opts ==) . steamID) playerStats 
  let kills' = fmap (kills :: PlayerStats -> Int) onePlayerStats
  let avgKills = (realToFrac $ sum kills') / realToFrac logCount
  print $ (show $ sum kills') <> " kills in " <> show logCount <> " matches (avg. " <>
    show avgKills <> ")"
  --print $ fmap classStats onePlayerStats

  closeConnection conn
