{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import GHC.Generics
-- TODO: For getField, it's less shit than annotating record access, upgrade to RecordDotSyntax when it's available
import GHC.Records
import Network.Http.Client
import Control.Exception
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Aeson hiding (Options)
import Data.Bits
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as T--(toUpper, toLower)
import Numeric
import OpenSSL
import Options.Generic
import System.ProgressBar
import Text.Read

addr = "logs.tf"

newtype Second = Second Int
newtype Minute = Minute Int

data Options = Options {
  steamID64 :: Int
, count :: Maybe Int
} deriving (Generic, ParseRecord, Show)

data MatchInfo = MatchInfo {
  length :: Int
, players :: [ParsedPlayerStats]
} deriving (Generic, FromJSON)

data ParsedPlayerStats = ParsedPlayerStats {
  steamID :: Text
, classStats :: [ParsedClassStats]
, kills :: Int
, deaths :: Int
, airshots :: Int
, ubers :: Int
, drops :: Int
, healingDone :: Int
, damageDealt :: Int
, damageTaken :: Int
} deriving (Generic)

data TFClass = Scout | Soldier | Pyro | Demoman | Heavy | Engineer | Medic | Sniper | Spy
  deriving (Eq, Ord, Read, Show)

instance Show Second where
  show (Second a) = (show a) <> "s"

instance Show Minute where
  show (Minute a) = (show a) <> "m"

instance {-# OVERLAPPING #-} FromJSON (Maybe TFClass) where
  parseJSON = withText "Maybe TFClass" $ \x -> do
    let s = T.unpack $ (T.toUpper $ T.take 1 x) <> T.drop 1 x
    result s where
      result "Heavyweapons" = pure $ Just Heavy
      result s = pure $ readMaybe s -- We might get the 10th class here...

data ParsedClassStats = ParsedClassStats {
  _type :: Maybe TFClass -- keyword as JSON field :(
, kills :: Int
, deaths :: Int
, playTime :: Second
} deriving (Generic)

-- Manual parser since aeson doesn't let me treat Maybe as mandatory...
instance FromJSON ParsedClassStats where
  parseJSON = withObject "ClassStats" $ \o -> do
    ParsedClassStats <$>
      o .: "type" <*>
      o .: "kills" <*>
      o .: "deaths" <*>
      (Second <$> o .: "total_time")
  
-- Shared per class stats and summed for all class stats
data BaseStats = BaseStats {
  kills :: Int
, deaths :: Int
, playTime :: Second
} deriving (Show)

-- Things we only get per player, not per class 
{-data OverallStats = OverallStats {
  damage
-}

data MedicStats = MedicStats {
  ubers :: Int
, drops :: Int
, healingDone :: Int
} deriving (Show)

newtype ExplosiveClassStats = Airshots Int deriving (Show)
newtype SniperStats = Headshots Int deriving (Show)
newtype SpyStats = Backstabs Int deriving (Show)

data ClassSpecificStats =
  MedicStats' MedicStats |
  ExplosiveClassStats' ExplosiveClassStats |
  SniperStats' SniperStats |
  SpyStats' SpyStats deriving (Show)

-- The parsed JSON might contain bad data (eg. 10th class)
data ClassStats = ClassStats {
  _type :: TFClass
, baseStats :: BaseStats
, classSpecificStats :: Maybe ClassSpecificStats
} deriving (Show)

-- Represents single match
data PlayerStats = PlayerStats {
  steamID :: Text
, baseStats :: BaseStats
, classStats :: [ClassStats]
} deriving (Show)

-- Stats normalized by time played
data PlayerStatsPresentation = PlayerStatsPresentation {
  playerStats :: PlayerStats
, samplePeriod :: Minute
}

data BaseStatPresentation = BaseStatPresentation {
  baseStats :: BaseStats
, samplePeriod :: Minute
}

data StatPresentation a = StatPresentation {
  stats :: a
, samplePeriod :: Minute
}

data LogQuery = LogQuery {
  success :: Bool
, results :: Int
, total :: Int -- Total logs for this player
, logs :: [PlayerLog]
} deriving (Show, Generic, FromJSON)

data PlayerLog = PlayerLog {
  id :: Int
} deriving (Show, Generic, FromJSON)

{-instance Show ClassStats where
  show x =
    (show $ getField @"_type" x) <> "\n" <>
    (indent $ show $ getField @"baseStats" x)

instance Show BaseStats where
  show x =
    "Kills: " <> (show $ getField @"kills" x) <> "\n" <>
    "Deaths: " <> (show $ getField @"deaths" x)
-}

{-
Scout (10 instances):
  Kills: 600 (30 per 30m)
-} 

safeInit [] = []
safeInit [_] = []
safeInit (x:xs) = x : safeInit xs

-- Produces a String eg. 'Kills: 90 (24.67 per 30m)
showTimeNormalizedStat :: (Real a, Show a) => String -> a -> Second -> Minute -> String
showTimeNormalizedStat statName x (Second playTime) (Minute period) =
  statName <> ": " <> (show x) <> " (" <> (showFFloat (Just 2) ( perM * (realToFrac period) ) ) "" <>
      " per " <> (show (Minute period) ) <> ")" where
    perM = ( (/) (realToFrac x) (realToFrac playTime) ) * 60

instance Show (StatPresentation BaseStats) where
  show (StatPresentation stats period) =
    "Playtime: " <> (showFFloat (Just 2) $ (realToFrac $ pts) / 60) "" <> "m\n" <>
    showTimeNormalizedStat "Kills" (getField @"kills" stats) pt period <> "\n" <>
    showTimeNormalizedStat "Deaths" (getField @"deaths" stats) pt period
    where
      pt = getField @"playTime" stats
      (Second pts) = getField @"playTime" stats

instance Show (StatPresentation ClassStats) where
  show (StatPresentation stats period) =
    show (getField @"_type" stats) <> ":\n" <>
    -- init drops the last newline that show StatPresentation adds
    (safeInit $ indent $ show $ StatPresentation (getField @"baseStats" stats) period) <>
    (showSpec $ getField @"classSpecificStats" stats)
    where
      showSpec Nothing = ""
      showSpec (Just (ExplosiveClassStats' (Airshots a))) =
        "\n\t" <> showTimeNormalizedStat "Airshots" a pt period
      showSpec (Just (MedicStats' ms)) =
        "\n\t" <> (showTimeNormalizedStat "Ubers" (getField @"ubers" ms) pt period) <> "\n\t" <>
        (showTimeNormalizedStat "Drops" (getField @"drops" ms) pt period) <> "\n\t" <>
        (showTimeNormalizedStat "Healing done" (getField @"healingDone" ms) pt period) 
      pt = getField @"playTime" $ getField @"baseStats" stats

instance Show (StatPresentation PlayerStats) where
  show (StatPresentation stats period) =
    (show $ StatPresentation (getField @"baseStats" stats) period) <> "\n" <>
    (unwords $ fmap (\s -> (show $ StatPresentation s period) <> "\n") (getField @"classStats" stats) ) 

instance {-# OVERLAPPING #-} FromJSON [ParsedPlayerStats] where
  parseJSON x = parseJSON x >>= mapM parseEntry . HML.toList where
    parseEntry (t, v) = withObject "[PlayerStats]" (\o ->
      ParsedPlayerStats t <$>
        o .: "class_stats" <*>
        o .: "kills" <*>
        o .: "deaths" <*>
        o .: "as" <*>
        o .: "ubers" <*>
        o .: "drops" <*>
        o .: "heal" <*>
        o .: "dmg" <*>
        o .: "dt") v
      --v is the JSON value containing the player objects with SteamID fields

sumClassSpecificStats :: ClassSpecificStats -> ClassSpecificStats -> Maybe ClassSpecificStats
sumClassSpecificStats (ExplosiveClassStats' (Airshots a)) (ExplosiveClassStats' (Airshots b)) =
  Just $ ExplosiveClassStats' $ Airshots $ a + b
sumClassSpecificStats (MedicStats' (MedicStats a b c) ) (MedicStats' (MedicStats a' b' c') ) =
  Just $ MedicStats' $ MedicStats (a + a') (b + b') (c + c')
sumClassSpecificStats _ _ = Nothing

sumClassStats :: ClassStats -> ClassStats -> Maybe ClassStats
sumClassStats x y =
  case (getField @"_type" x == getField @"_type" y) of
    False -> Nothing
    True -> Just $ ClassStats {
        _type = getField @"_type" x
      , baseStats = sumBaseStats (getField @"baseStats" x) (getField @"baseStats" y)
      , classSpecificStats = sumCStats' (getField @"classSpecificStats" x) (getField @"classSpecificStats" y)
    } where
      sumCStats' x y = do
        m <- x
        n <- y
        sumClassSpecificStats m n

sumClassStatsLists :: [ClassStats] -> [ClassStats] -> [ClassStats]
sumClassStatsLists xs ys =
  -- thanks to joel135 from #haskell
  catMaybes $ M.elems $ M.unionWith sumStats' (toMap xs) (toMap ys) where
    toMap zs = M.fromList [(getField @"_type" s, Just s) | s <- zs]
    sumStats' x y = do
      m <- x
      n <- y
      sumClassStats m n

sumBaseStats :: BaseStats -> BaseStats -> BaseStats
sumBaseStats (BaseStats a b (Second c)) (BaseStats a' b' (Second c')) =
  BaseStats (a + a') (b + b') (Second (c + c'))

-- TODO: doesn't care about SteamID, so only use on lists that have the same one
sumPlayerStats :: PlayerStats -> PlayerStats -> PlayerStats
sumPlayerStats x y =
  PlayerStats {
    steamID = getField @"steamID" x
  , baseStats = sumBaseStats (getField @"baseStats" x) (getField @"baseStats" y)
  , classStats = sumClassStatsLists (getField @"classStats" x) (getField @"classStats" y)
  }

parsedPlayerStatsToBaseStats :: ParsedPlayerStats -> BaseStats
parsedPlayerStatsToBaseStats x =
  BaseStats {
    kills = getField @"kills" x
  , deaths = getField @"deaths" x
  , playTime = foldl1 (\(Second y) (Second z) -> Second (y + z)) playTimes
  } where
    playTimes = fmap (getField @"playTime") (getField @"classStats" x)

parsedClassStatsToBaseStats :: ParsedClassStats -> BaseStats
parsedClassStatsToBaseStats x =
  BaseStats {
    kills = getField @"kills" x
  , deaths = getField @"deaths" x
  , playTime = getField @"playTime" x
  }

-- Move class specific stats from ParsedPlayerStats to ClassStats
toClassStats :: ParsedPlayerStats -> ParsedClassStats -> Maybe ClassStats
toClassStats playerStats classStats
  = clss >>= \c -> Just $ ClassStats {
      _type = c
    , baseStats = parsedClassStatsToBaseStats classStats
    , classSpecificStats = specificStats c
  }
  where  
    clss = getField @"_type" classStats
    -- NOTE: airshots aren't separated by class so this is inaccurate!
    specificStats Soldier = Just $ ExplosiveClassStats' $ Airshots $ getField @"airshots" playerStats
    specificStats Demoman = Just $ ExplosiveClassStats' $ Airshots $ getField @"airshots" playerStats
    specificStats Medic = Just $ MedicStats' $ MedicStats {
        ubers = getField @"ubers" playerStats
      , drops = getField @"drops" playerStats
      , healingDone = getField @"healingDone" playerStats
    }
    specificStats _ = Nothing

fromParsedPlayerStats :: ParsedPlayerStats -> PlayerStats
fromParsedPlayerStats parsedStats =
  PlayerStats {
    steamID = getField @"steamID" parsedStats
  , baseStats = parsedPlayerStatsToBaseStats parsedStats
  , classStats = catMaybes $ fmap (toClassStats parsedStats) (getField @"classStats" parsedStats)
  }

playerLogs :: Connection -> ByteString -> IO LogQuery
playerLogs conn steamID = do
  let path = "/api/v1/log?player=" <> steamID
  let req = buildRequest1 $ (do
        http GET path
        setAccept "application/json")
  
  sendRequest conn req emptyBody
  receiveResponse conn jsonHandler :: IO LogQuery

-- Get log (MatchInfo) by id
matchInfo :: Connection -> ByteString -> IO (Maybe MatchInfo)
matchInfo conn id = do
  let req = buildRequest1 $ (do
        http GET $ "/json/" <> id
        setAccept "application/json")
  threadDelay 100000 --Delay since logs.tf doesn't seem to like the frequent requests
  sendRequest conn req emptyBody
  
  res <- catch ((receiveResponse conn jsonHandler :: IO MatchInfo) >>= pure . Just)
    (\(_ :: SomeException) -> pure Nothing)
  pure res

toByteString :: Show a => a -> ByteString
toByteString x = BS.pack $ show x

-- Indent all lines of string for pretty printing
indent = unlines . fmap ("\t" <>) . lines

-- https://developer.valvesoftware.com/wiki/SteamID
steamID64toSteamID3 :: Int -> Text
steamID64toSteamID3 x =
  "[U:1:" <> (T.pack $ show $ (highBits * 2) + lowBit) <> "]" where
    highBits = (shiftR x 1) .&. 134217727 -- 0x7FFFFFF
    lowBit = x .&. 1

{-
-- Separate function to show class specific stats
-- Make sure the list only contains ClassStats whose class is the same
showClassSummary :: [ParsedClassStats] -> String
showClassSummary stats =
  (show $ Data.Foldable.length stats) <> " instances of " <> (show $ getField @"_type" $ head stats) <>
  " with " <> (show $ sum $ fmap (getField @"kills") stats) <> " kills"

showClassStats :: [ParsedClassStats] -> String
showClassStats stats =
  let classSorted = sortBy (\x y -> compare (getField @"_type" x) (getField @"_type" y)) stats 
      classSeparated = groupBy (\x y -> getField @"_type" x == getField @"_type" y) classSorted
      classSummaries = unwords $ fmap ((<> "\n") . showClassSummary) classSeparated
  in show classSummaries

showStats :: [ParsedPlayerStats] -> String
showStats playerStats =
  let classStats' = join $ fmap (getField @"classStats") playerStats
  in showClassStats classStats'
-}

showStats :: [PlayerStats] -> String
showStats xs =
  -- xs.baseStats.deaths 
  show $ sum $ fmap (\x -> getField @"kills" $ getField @"baseStats" x) xs

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

  let playerStats = join $ fmap players $ catMaybes stats
  let steamID3' = steamID64toSteamID3 $ steamID64 opts
  print steamID3'
  let onePlayerStats = filter ((steamID3' ==) . getField @"steamID") playerStats 
  --let kills' = fmap (getField @"kills") onePlayerStats
  --let avgKills = (realToFrac $ sum kills') / realToFrac logCount
  --print $ (show $ sum kills') <> " kills in " <> show logCount <> " matches (avg. " <>
    --show avgKills <> ")"
  --putStrLn $ showStats onePlayerStats
  --print $ showStats $ fmap fromParsedPlayerStats onePlayerStats
  --print $ getField @"baseStats" $ head $ fmap fromParsedPlayerStats onePlayerStats
  let pureStats = fmap fromParsedPlayerStats onePlayerStats
  --putStr $ show $ head $ fmap fromParsedPlayerStats onePlayerStats
  putStr $ show $ foldl1 sumPlayerStats pureStats
  putStr $ show $ StatPresentation (foldl1 sumPlayerStats pureStats) (Minute 30)

  closeConnection conn
