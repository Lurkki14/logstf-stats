{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

import GHC.Generics
import Network.Http.Client
import Control.Concurrent (threadDelay)
import Control.Monad (join)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Binary as B
import Data.ByteString
import qualified Data.ByteString.Char8 as BS
import Data.HashMap.Lazy
import Data.Text
import OpenSSL

testPath = "/json/2752512"

addr = "logs.tf"

--steamID = "[U:1:37776901]"

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
  --n :: Text, 
  id :: Int
} deriving (Show, Generic, FromJSON)

instance {-# OVERLAPPING #-} FromJSON [PlayerStats] where
  parseJSON x = parseJSON x >>= mapM parseEntry . toList where
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
  --print $ show id
  sendRequest conn req emptyBody
  receiveResponse conn jsonHandler :: IO MatchInfo

main = withOpenSSL $ do
  ctx <- baselineContextSSL
  conn <- openConnectionSSL ctx addr 443
  
  logQuery <- playerLogs conn "76561198078484073"
  let ids = fmap Main.id $ logs $ logQuery 
  let binIDs = fmap (BS.pack . show) ids
  let matchInfosM = fmap (matchInfo conn) binIDs
  let logs10 = Prelude.take 20 matchInfosM
  stats <- sequence logs10
  let playerStats = join $ fmap players stats
  let onePlayerStats = Prelude.filter (\ps -> steamID ps == "[U:1:118218345]") playerStats 
  let kills' = fmap kills onePlayerStats
  print $ (show $ sum kills') <> " kills in 20 matches"

  closeConnection conn
