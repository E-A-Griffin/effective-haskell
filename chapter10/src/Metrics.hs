{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Metrics where

import Data.Foldable (for_)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Time.Clock
  ( diffUTCTime
  , getCurrentTime
  , nominalDiffTimeToSeconds
  )
import Text.Printf (printf)

data AppMetrics = AppMetrics
  { successCount :: Int
  , failureCount :: Int
  , callDuration :: Map.Map String Int
  } deriving (Eq, Show)

newtype Metrics = Metrics { appMetricsStore :: IORef AppMetrics }

newMetrics :: IO Metrics
newMetrics =
  let
    emptyAppMetrics = AppMetrics
      { successCount = 0
      , failureCount = 0
      , callDuration = Map.empty
      }
  in Metrics <$> newIORef emptyAppMetrics

tickSuccess :: Metrics -> IO ()
tickSuccess (Metrics metricsRef) = modifyIORef' metricsRef $ \m ->
  m { successCount = 1 + successCount m }

tickFailure :: Metrics -> IO ()
tickFailure (Metrics metricsRef) = modifyIORef' metricsRef $ \m ->
  m { successCount = 1 + failureCount m }

timeFunction :: Metrics -> String -> IO a -> IO a
timeFunction (Metrics metrics) actionName action = do
  startTime <- getCurrentTime
  result    <- action
  endTime   <- getCurrentTime

  modifyIORef' metrics $ \oldMetrics ->
    let
      oldDurationValue =
        fromMaybe 0 $ Map.lookup actionName (callDuration oldMetrics)

      runDuration =
        floor . (* 100000) . nominalDiffTimeToSeconds $
          diffUTCTime endTime startTime

      newDurationValue = oldDurationValue + runDuration

    in
      oldMetrics {
        callDuration =
            Map.insert actionName newDurationValue $
              callDuration oldMetrics
        }

  pure result

displayMetrics :: Metrics -> IO ()
displayMetrics (Metrics metricsStore) = do
  AppMetrics{..} <- readIORef metricsStore
  putStrLn $ "successes: " <> show successCount
  putStrLn $ "failures: " <> show failureCount
  for_ (Map.toList callDuration) $ \(functionName, timing :: Int) ->
    putStrLn $ printf "Time spent in \"%s\": %d" functionName timing
