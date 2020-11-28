{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import GHC.Generics
import Network.Http.Client
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Aeson hiding (Options)
import Data.Bits
import Data.Foldable
import Data.List
import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as T--(toUpper, toLower)
import OpenSSL
import Options.Generic
import System.ProgressBar
import Text.Read

addr = "logs.tf"

data Options = Options {
  steamID64 :: Int
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
  deriving (Eq, Ord, Read, Show)

instance {-# OVERLAPPING #-} FromJSON (Maybe TFClass) where
  parseJSON = withText "Maybe TFClass" $ \x -> do
    let s = T.unpack $ (T.toUpper $ T.take 1 x) <> T.drop 1 x
    result s where
      result "Heavyweapons" = pure $ Just Heavy
      result s = pure $ readMaybe s -- We might get the 10th class here...

data ClassStats = ClassStats {
  _type :: Maybe TFClass -- keyword as JSON field :(
, kills :: Int
, deaths :: Int
} deriving (Generic, Show)

-- Manual parser since aeson doesn't let me treat Maybe as mandatory...
instance FromJSON ClassStats where
  parseJSON = withObject "ClassStats" $ \o -> do
    ClassStats <$>
      o .: "type" <*>
      o .: "kills" <*>
      o .: "deaths"
  
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
  
  {-let handleJSON = \r i -> do
      print $ getStatusCode r
      jsonHandler r i :: IO MatchInfo 
  res <- try $ (receiveResponse conn jsonHandler :: IO MatchInfo)
  case (res :: Either IOError MatchInfo) of
    Left _ -> error "Couldn't parse JSON!"
    Right v -> return v-}

  receiveResponse conn jsonHandler :: IO MatchInfo

toByteString :: Show a => a -> ByteString
toByteString x = BS.pack $ show x

-- https://developer.valvesoftware.com/wiki/SteamID
steamID64toSteamID3 :: Int -> Text
steamID64toSteamID3 x =
  "[U:1:" <> (T.pack $ show $ highBits * 2) <> "]" where
  highBits = (shiftR x 1) .&. 134217727 -- 0x7FFFFFF 

-- Separate function to show class specific stats
-- Make sure the list only contains ClassStats whose class is the same
showClassSummary :: [ClassStats] -> String
showClassSummary stats =
  (show $ Data.Foldable.length stats) <> " instances of " <> (show $ _type $ head stats) <>
  " with " <> (show $ sum $ fmap (kills :: ClassStats -> Int) stats) <> " kills"

showClassStats :: [ClassStats] -> String
showClassStats stats =
  let classSorted = sortBy (\x y -> compare (_type x) (_type y)) stats 
      classSeparated = groupBy (\x y -> _type x == _type y) classSorted
      classSummaries = unwords $ fmap ((<> "\n") . showClassSummary) classSeparated
  in show classSummaries

showStats :: [PlayerStats] -> String
showStats playerStats =
  let classStats' = join $ fmap classStats playerStats
  in showClassStats classStats'

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
  let steamID3' = steamID64toSteamID3 $ steamID64 opts
  let onePlayerStats = filter ((steamID3' ==) . steamID) playerStats 
  let kills' = fmap (kills :: PlayerStats -> Int) onePlayerStats
  let avgKills = (realToFrac $ sum kills') / realToFrac logCount
  print $ (show $ sum kills') <> " kills in " <> show logCount <> " matches (avg. " <>
    show avgKills <> ")"
  putStrLn $ showStats onePlayerStats

  closeConnection conn
