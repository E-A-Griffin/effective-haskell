{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (IOException, handle)
import Control.Monad (unless)
import Control.Monad.Extra (concatForM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isSuffixOf)
import Data.Map qualified as Map
import Data.Set qualified as Set (empty, insert, member)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import System.Directory
  ( canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import Metrics
import System.Environment (getArgs)
import Text.Printf (printf)

data FileType
  = FileTypeDirectory
  | FileTypeRegularFile
  | FileTypeOther
  deriving (Eq)

classifyFile :: FilePath -> IO FileType
classifyFile fname = do
  isDirectory <- doesDirectoryExist fname
  isFile <- doesFileExist fname
  pure $ case (isDirectory, isFile) of
           (True, False) -> FileTypeDirectory
           (False, True) -> FileTypeRegularFile
           _otherwise    -> FileTypeOther

countBytes :: FilePath -> IO (FilePath, Integer)
countBytes path = do
  bytes <- fromIntegral . BS.length <$> BS.readFile path
  pure (path, bytes)

naiveTraversal :: FilePath -> (FilePath -> a) -> IO [a]
naiveTraversal rootPath action = do
  classification <- classifyFile rootPath
  case classification of
    FileTypeOther ->
      pure []
    FileTypeRegularFile ->
      pure [action rootPath]
    FileTypeDirectory -> do
      contents <- map (fixPath rootPath) <$> listDirectory rootPath
      concat <$> getPaths contents
  where
    fixPath parent fname = parent <> "/" <> fname
    getPaths = mapM (\path -> naiveTraversal path action)

traverseDirectory :: Metrics -> FilePath -> (FilePath -> IO ()) -> IO ()
traverseDirectory metrics rootPath action = do
  seenRef <- newIORef Set.empty
  let
    haveSeenDirectory canonicalPath =
      Set.member canonicalPath <$> readIORef seenRef

    addDirectoryToSeen canonicalPath =
      modifyIORef' seenRef $ Set.insert canonicalPath

    handler ex = print ex >> tickFailure metrics

    traverseSubdirectory subdirPath =
      timeFunction metrics "traverseSubdirectory" $ do
        contents <- listDirectory subdirPath
        for_ contents $ \file' ->
          handle @IOException handler $ do
          let file = subdirPath <> "/" <> file'
          canonicalPath <- canonicalizePath file
          classification <- classifyFile canonicalPath
          result <- case classification of
            FileTypeOther -> pure ()
            FileTypeRegularFile ->
              action file
            FileTypeDirectory -> do
              alreadyProcessed <- haveSeenDirectory file
              unless alreadyProcessed $ do
                addDirectoryToSeen file
                traverseSubdirectory file
          tickSuccess metrics
          pure result

  traverseSubdirectory (dropSuffix "/" rootPath)


traverseDirectory' :: MetricsStore -> FilePath -> (FilePath -> IO ()) -> IO ()
traverseDirectory' metrics rootPath action = do
  seenRef <- newIORef Set.empty
  let
    haveSeenDirectory canonicalPath =
      Set.member canonicalPath <$> readIORef seenRef

    addDirectoryToSeen canonicalPath =
      modifyIORef' seenRef $ Set.insert canonicalPath

    handler ex = print ex >> storeTickFailure metrics

    traverseSubdirectory subdirPath =
      storeTimeFunction metrics "traverseSubdirectory" $ do
        contents <- listDirectory subdirPath
        for_ contents $ \file' ->
          handle @IOException handler $ do
          let file = subdirPath <> "/" <> file'
          canonicalPath <- canonicalizePath file
          classification <- classifyFile canonicalPath
          result <- case classification of
            FileTypeOther -> pure ()
            FileTypeRegularFile ->
              action file
            FileTypeDirectory -> do
              alreadyProcessed <- haveSeenDirectory file
              unless alreadyProcessed $ do
                addDirectoryToSeen file
                traverseSubdirectory file
          storeTickSuccess metrics
          pure result

  traverseSubdirectory (dropSuffix "/" rootPath)

traverseDirectoryIO :: forall a. FilePath -> (FilePath -> IO a) -> IO [a]
traverseDirectoryIO rootPath action = do
  seenRef <- newIORef Set.empty
  let
    haveSeenDirectory canonicalPath =
      Set.member canonicalPath <$> readIORef seenRef

    addDirectoryToSeen canonicalPath =
      modifyIORef' seenRef $ Set.insert canonicalPath

    handler ex = print ex >> pure []

    traverseSubdirectory :: FilePath -> IO [a]
    traverseSubdirectory subdirPath = do
        contents <- listDirectory subdirPath
        concatForM contents $ \file' ->
          handle @IOException handler $ do
          let file = subdirPath <> "/" <> file'
          canonicalPath <- canonicalizePath file
          classification <- classifyFile canonicalPath
          result <- case classification of
            FileTypeOther -> pure []
            FileTypeRegularFile ->
              sequence [action file]
            FileTypeDirectory -> do
              alreadyProcessed <- haveSeenDirectory file
              if alreadyProcessed then do
                addDirectoryToSeen file
                traverseSubdirectory file
              else
                pure []
          pure result

  traverseSubdirectory (dropSuffix "/" rootPath)


longestContents :: Metrics -> FilePath -> IO ByteString
longestContents metrics rootPath = do
  contentsRef <- newIORef BS.empty
  let
    takeLongestFile a b =
      if BS.length a >= BS.length b
      then a
      else b

  traverseDirectory metrics rootPath $ \file -> do
    contents <- BS.readFile file
    modifyIORef' contentsRef (takeLongestFile contents)

  readIORef contentsRef

dropSuffix :: String -> String -> String
dropSuffix suffix s
  | suffix `isSuffixOf` s =
    take (length s - length suffix) s
  | otherwise = s

writeIORef' :: IORef a -> a -> IO ()
writeIORef' ref !val = writeIORef ref val

directorySummaryWithMetrics :: FilePath -> IO ()
directorySummaryWithMetrics root = do
  metrics <- newMetrics
  histogramRef <- newIORef Map.empty

  traverseDirectory metrics root $ \file -> do
    putStrLn $ file <> ":"
    contents <- timeFunction metrics "TextIO.readFile" $
      TextIO.readFile file

    timeFunction metrics "wordcount" $
      let wordCount = length $ Text.words contents
      in putStrLn $ "    word count: " <> show wordCount

    timeFunction metrics "histogram" $ do
      oldHistogram <- readIORef histogramRef
      let
        addCharToHistogram histogram letter =
          Map.insertWith (+) letter 1 histogram
        !newHistogram = Text.foldl' addCharToHistogram oldHistogram contents
      modifyIORef' histogramRef (const newHistogram)

  histogram <- readIORef histogramRef
  putStrLn "Histogram Data:"
  for_ (Map.toList histogram) $ \(letter :: Char, count :: Int) ->
    printf "    %c: %d\n" letter count

  displayMetrics metrics

directorySummaryWithMetrics' :: FilePath -> IO ()
directorySummaryWithMetrics' root = do
  metrics <- newMetricsStore
  histogramRef <- newIORef Map.empty

  traverseDirectory' metrics root $ \file -> do
    putStrLn $ file <> ":"
    contents <- storeTimeFunction metrics "TextIO.readFile" $
      TextIO.readFile file

    storeTimeFunction metrics "wordcount" $
      let wordCount = length $ Text.words contents
      in putStrLn $ "    word count: " <> show wordCount

    storeTimeFunction metrics "histogram" $ do
      oldHistogram <- readIORef histogramRef
      let
        addCharToHistogram histogram letter =
          Map.insertWith (+) letter 1 histogram
        !newHistogram = Text.foldl' addCharToHistogram oldHistogram contents
      modifyIORef' histogramRef (const newHistogram)

  histogram <- readIORef histogramRef
  putStrLn "Histogram Data:"
  for_ (Map.toList histogram) $ \(letter :: Char, count :: Int) ->
    printf "    %c: %d\n" letter count

  displayMetricsStore metrics

-- Conclusion both approaches have similar performance but whichever runs second
-- does better likely due to caching
main :: IO ()
main = getArgs
  >>= (\args@(arg:_) -> directorySummaryWithMetrics' arg >> pure args)
  >>= directorySummaryWithMetrics . head
