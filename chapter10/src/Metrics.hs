{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

data MetricsStore = MetricsStore
  { storeSuccessCount :: IORef Int
  , storeFailureCount :: IORef Int
  , storeCallDuration :: IORef (Map.Map String Int)
  } deriving (Eq)

newMetrics :: IO Metrics
newMetrics =
  let
    emptyAppMetrics = AppMetrics
      { successCount = 0
      , failureCount = 0
      , callDuration = Map.empty
      }
  in Metrics <$> newIORef emptyAppMetrics

newMetricsStore :: IO MetricsStore
newMetricsStore = do
  successCount <- newIORef 0
  failureCount <- newIORef 0
  callDuration <- newIORef @(Map.Map String Int) Map.empty
  pure MetricsStore { storeSuccessCount = successCount
                    , storeFailureCount = failureCount
                    , storeCallDuration = callDuration
                    }

storeTickSuccess :: MetricsStore -> IO ()
storeTickSuccess (MetricsStore storeSuccessCount _ _) =
  modifyIORef' storeSuccessCount (+1)

storeTickFailure :: MetricsStore -> IO ()
storeTickFailure (MetricsStore _ storeFailureCount _) =
  modifyIORef' storeFailureCount (+1)

storeTimeFunction :: MetricsStore -> String -> IO a -> IO a
storeTimeFunction (MetricsStore _ _ callDuration) actionName action = do
  startTime <- getCurrentTime
  result    <- action
  endTime   <- getCurrentTime

  modifyIORef' callDuration $ \oldCallDuration -> do
    let
      oldDurationValue =
        fromMaybe 0 $ Map.lookup actionName oldCallDuration

      runDuration =
        floor . (* 100000) . nominalDiffTimeToSeconds $
          diffUTCTime endTime startTime

      newDurationValue = oldDurationValue + runDuration

    Map.insert actionName newDurationValue oldCallDuration

  pure result


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

timePureFunction :: Metrics -> String -> a -> IO a
timePureFunction (Metrics metrics) valueName value = do
  startTime <- getCurrentTime
  -- Strict evaluation to ensure we're timing how long it takes to actually
  -- evaluate value
  let !result = value
  endTime   <- getCurrentTime

  modifyIORef' metrics $ \oldMetrics ->
    let
      oldDurationValue =
        fromMaybe 0 $ Map.lookup valueName (callDuration oldMetrics)

      runDuration =
        floor . (* 100000) . nominalDiffTimeToSeconds $
          diffUTCTime endTime startTime

      newDurationValue = oldDurationValue + runDuration

    in
      oldMetrics {
        callDuration =
            Map.insert valueName newDurationValue $
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

displayMetricsStore :: MetricsStore -> IO ()
displayMetricsStore (MetricsStore successCount failureCount callDuration) =
  readIORef successCount
  >>= putStrLn . ("successes: " <>) . show
  >>  readIORef failureCount
  >>= putStrLn . ("failures: " <>) . show
  >>  readIORef callDuration
  >>= \cd -> for_ (Map.toList cd) $ \(functionName, timing) ->
                                      putStrLn ( printf "Time spend in \"%s\": %d"
                                                 functionName
                                                 timing
                                               )
