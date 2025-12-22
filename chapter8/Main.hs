{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Pager!

module Main where

import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Time.Clock qualified as Clock
import Data.Time.Clock.POSIX qualified as PosixClock
import Data.Time.Format qualified as TimeFormat
import System.Directory qualified as Directory
import System.Info qualified as SystemInfo
import System.Process (readProcess)
import GHC.IO.Handle (hSetBuffering)

data ContinueCancel = Continue | Cancel deriving (Eq, Show)

data FileInfo = FileInfo
  { filePath :: FilePath
  , fileSize :: Int
  , fileMTime :: Clock.UTCTime
  , fileReadable :: Bool
  , fileWriteable :: Bool
  , fileExecutable :: Bool
  } deriving Show

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int
  , screenColumns :: Int
  } deriving Show

eitherToErr :: Show a => Either a b -> IO b
eitherToErr (Right a) = return a
eitherToErr (Left e) =
  Exception.throwIO . IOError.userError $ show e

fileInfo :: FilePath -> IO FileInfo
fileInfo filePath = do
  perms <- Directory.getPermissions filePath
  mtime <- Directory.getModificationTime filePath
  contents <- BS.readFile filePath
  let size = BS.length contents
  return FileInfo
    { filePath
    , fileSize
    , fileMTime
    , fileReadable = Directory.readable perms
    , fileWriteable = Directory.writeable perms
    , fileExecutable = Directory.executable perms
    }

groupsOf :: Int -> [a] -> [[a]]
groupsOf n elems =
  let (hd, tl) = splitAt n elems
  in hd : groupsOf n tl

wordWrap :: Int -> Text.Text -> [Text.Text]
wordWrap lineLength lineText
  | Text.length lineText <= lineLength = [lineText]
  | otherwise =
    let
      (candidate, nextLines) = Text.splitAt lineLength lineText
      (firstLine, overflow) = softWrap candidate (Text.length candidate - 1)
    in firstLine : wordWrap lineLength (overflow <> nextLines)
  where
    softWrap hardwrappedText textIndex
      | textIndex <= 0 = (hardwrappedText,Text.empty)
      | Text.index hardwrappedText textIndex == ' ' =
        let (wrappedLine, rest) = Text.splitAt textIndex hardwrappedText
        in (wrappedLine, Text.tail rest)
      | otherwise = softWrap hardwrappedText (textIndex - 1)

formatFileInfo :: FileInfo -> Int -> Int -> Int -> Text.Text
formatFileInfo FileInfo{..} maxWidth totalPages currentPage =
  let
    timestamp =
      TimeFormat.formatTime TimeFormat.defaultTimeLocale "%F %T" fileMTime
    permissionString =
      [ if fileReadable then 'r' else '-'
      , if fileWriteable then 'w' else '-'
      , if fileExecutable then 'x' else '-' ]
    statusLine = Text.pack $
      printf
      "%s | permissions: %s | %d bytes | modified: %s | page: %d of %d"
      filePath
      permissionString
      fileSize
      timestamp
      currentPage
      totalPages
  in invertText (truncateStatus statusLine)
  where
    invertText inputStr =
      let
        reverseVideo = "\^[[7m"
        resetVideo = "\^[[0m"
      in reverseVideo <> inputStr <> resetVideo
    truncateStatus statusLine
      | maxWidth <= 3 = ""
      | Text.length statusLine > maxWidth =
        Text.take (maxWidth - 3) statusLine <> "..."
      | otherwise = statusLine

paginate :: ScreenDimensions -> FileInfo -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows cols) fInfo text =
  let
    rows' = rows - 1
    wrappedLines = concatMap (wordWrap cols) (Text.lines text)
    pages = map (Text.unlines . padTo rows') $ groupsOf rows' wrappedLines
    pageCount = length pages
    statusLines = map (formatFileInfo fInfo cols pageCount) [1..pageCount]
  in zipWith (<>) pages statusLines
  where
    padTo :: Int -> [Text.Text] -> [Text.Text]
    padTo lineCount rowsToPad =
      take lineCount $ rowsToPad <> repeat ""


getTerminalSize :: IO ScreenDimensions
getTerminalSize =
  case SystemInfo.os of
    "darwin" -> tputScreenDimensions
    "linux" -> tputScreenDimensions
    _other -> pure $ ScreenDimensions 25 80
  where
    -- TODO: Add error handling
    tputScreenDimensions :: IO Dimensions
    tputScreenDimensions =
      readProcess "tput" ["lines"] ""
      >>= \lines ->
        readProcess "tput" ["cols"] ""
        >>= \cols ->
              let lines' = read $ init lines
                  cols' = read $ init cols
              in return $ ScreenDimensions lines' cols'

getContinue :: IO ContinueCancel
getContinue =
  hSetBuffering stdin NoBuffering
  >> hSetEcho stdin False
  >> hGetChar stdin
  >>= \input ->
    case input of
      ' ' -> return Continue
      'q' -> return Cancel
      _   -> getContinue

clearScreen :: IO ()
clearScreen =
  BS.putStr "\^[[1J\^[[1;1H"

showPages :: [Text.Text] -> IO ()
showPages [] = return ()
showPages (page:pages) =
  clearScreen
  >> TextIO.putStr page
  >> getContinue
  >>= \case
    Continue -> showPages pages
    Cancel   -> return ()

runHCat :: IO ()
runHCat = do
  args <- handleArgs
  targetFilePath <- eitherToErr args
  contents <- TextIO.hGetContents <<= openFile targetFilePath ReadMode
  termSize <- getTerminalSize
  hSetBuffering stdout NoBuffering
  fInfo <- fileInfo targetFilePath
  let pages = paginate termSize fInfo contents
  showPages pages

main :: IO ()
main = runHCat
