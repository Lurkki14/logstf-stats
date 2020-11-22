{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

import GHC.Generics
import Network.Http.Client
import Control.Concurrent (threadDelay)
import Control.Monad (join)
import Data.Aeson hiding (Options)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Lazy as HML
import Data.Text ()
import OpenSSL
import Options.Generic

addr = "logs.tf"

data Options = Options {
  steamID64 :: Int
, steamID3 :: Text
} deriving (Generic, ParseRecord, Show)

data MatchInfo = MatchInfo {
  length :: Int
, players :: [PlayerStats]
} deriving (Show, Generic, FromJSON)

data PlayerStats = PlayerStats {
  steamID :: Text
, kills :: Int
, deaths :: Int
} deriving (Show)

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
      PlayerStats t <$> o .: "kills" <*> o .: "deaths") v
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
  let logs10 = Prelude.take 30 matchInfosM
  stats <- sequence logs10
  let playerStats = join $ fmap players stats
  let onePlayerStats = filter ((steamID3 opts ==) . steamID) playerStats 
  let kills' = fmap kills onePlayerStats
  print $ (show $ sum kills') <> " kills in 20 matches"

  closeConnection conn
