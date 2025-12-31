{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Pager!

module Main where

import Control.Exception qualified as Exception
import Control.Monad (unless, void, when)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Time.Clock qualified as Clock
import Data.Time.Format qualified as TimeFormat
import Debug.Trace qualified as Debug
import GHC.IO.Handle (hSetBuffering)
import System.Directory qualified as Directory
import System.Environment qualified as Env
import System.Info qualified as SystemInfo
import System.IO (
  BufferMode(..),
  IOMode(..),
  hGetChar,
  hPutStr,
  hSetEcho,
  openFile,
  stdin,
  stderr,
  stdout
  )
import System.IO.Error qualified as IOError
import System.Process (readProcess)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Command = Back | Cancel | Continue | Help | Resize deriving (Eq, Show)

data CharPos = CharPos
  { pageIdx :: Int
  , firstWordOfPage :: Bool
  }

data Progress = Progress
  { firstWordIdx :: Int -- index (in characters) of firstWord of current terminal
  , firstWord :: Text.Text
  , curPage :: Int
  }

data FileInfo = FileInfo
  { filePath :: FilePath
  , fileSize :: Int
  , fileMTime :: Clock.UTCTime
  , fileReadable :: Bool
  , fileWritable :: Bool
  , fileExecutable :: Bool
  } deriving Show

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int
  , screenColumns :: Int
  } deriving Show

eitherToErr :: Show a => Either a b -> IO b
eitherToErr (Right b) = return b
eitherToErr (Left e) =
  Exception.throwIO . IOError.userError $ show e

fileInfo :: FilePath -> IO FileInfo
fileInfo filePath = do
  perms <- Directory.getPermissions filePath
  mtime <- Directory.getModificationTime filePath
  contents <- BS.readFile filePath
  let size = BS.length contents
  pure FileInfo
    { filePath
    , fileSize = size
    , fileMTime = mtime
    , fileReadable = Directory.readable perms
    , fileWritable = Directory.writable perms
    , fileExecutable = Directory.executable perms
    }

groupsOf :: Show a => Int -> [a] -> [[a]]
groupsOf n elems =
  let (hd, tl) = splitAt (Debug.traceShowId n) (Debug.traceShowId elems)
  in Debug.trace "Getting hd: " (if null tl then [hd] else hd : groupsOf n tl)

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

formatFileInfo :: FileInfo -> Int -> Int -> Int -> Int -> Int -> Text.Text
formatFileInfo FileInfo{..} maxWidth totalFiles currentFile totalPages currentPage =
  let
    timestamp =
      TimeFormat.formatTime TimeFormat.defaultTimeLocale "%F %T" fileMTime
    permissionString =
      [ if fileReadable then 'r' else '-'
      , if fileWritable then 'w' else '-'
      , if fileExecutable then 'x' else '-' ]
    statusLine = Text.pack $
      printf
      "%s | permissions: %s | %d bytes | modified: %s | page: %d of %d | file: %d of %d"
      filePath
      permissionString
      fileSize
      timestamp
      currentPage
      totalPages
      currentFile
      totalFiles
  in Debug.trace "formatting FileInfo" $ invertText (truncateStatus statusLine)
  where
    invertText inputStr =
      let
        reverseVideo = "\^[[7m"
        resetVideo = "\^[[0m"
      in Debug.trace "reversing video" $ reverseVideo <> inputStr <> resetVideo
    truncateStatus statusLine
      | maxWidth <= 3 = Debug.trace "<= 3" ""
      | Text.length statusLine > maxWidth =
        Debug.trace "take take take" $ Text.take (maxWidth - 3) statusLine <> "..."
      | otherwise = Debug.trace "fallthrough" statusLine

-- Assumes `fileIdx` is 0-indexed and automatically converts to 1-indexed for display
paginate :: Int -> Int -> ScreenDimensions -> FileInfo -> Text.Text -> [Text.Text]
paginate fileIdx nFiles (ScreenDimensions rows cols) fInfo text =
  let
    rows' = Debug.trace "debugging" $ rows - 1
    wrappedLines = concatMap (wordWrap cols) (Text.lines text)
    pages = Debug.trace ("mapping on " <> show rows' <> " rows and " <> show wrappedLines <> " lines") $ map (Text.unlines . padTo rows') $ groupsOf rows' wrappedLines
    pageCount = Debug.trace ("page count: " <> show (length pages)) $ length pages
    statusLines = Debug.trace ("statussing on " <> show (length [1..pageCount]) <> " pages") $ map (formatFileInfo fInfo cols nFiles (succ fileIdx) pageCount) [1..pageCount]
  in Debug.trace "zipping" (zipWith (<>) pages statusLines)
  where
    padTo :: Int -> [Text.Text] -> [Text.Text]
    padTo lineCount rowsToPad =
      take lineCount $ rowsToPad <> repeat ""

getAnyKey :: IO Char
getAnyKey =
  hSetBuffering stdin NoBuffering
  >> hSetEcho stdin False
  >> hGetChar stdin

-- Prompt user with stderr message and request keystroke to acknowledge
proceedError :: String -> IO ()
proceedError msg =
  hPutStr stderr msg >>
  void getAnyKey

getTerminalSize :: IO ScreenDimensions
getTerminalSize =
  case SystemInfo.os of
      "darwin" -> tputScreenDimensions
      "linux" -> tputScreenDimensions
      _other -> pure $ ScreenDimensions defaultLines defaultCols
  where
    defaultLines :: Int
    defaultLines = 25

    defaultCols :: Int
    defaultCols = 80

    tputScreenDimensions :: IO ScreenDimensions
    tputScreenDimensions = do
      res :: ScreenDimensions <- Exception.catch (
        readProcess "tput" ["lines"] ""
        >>= \lines' ->
            readProcess "tput" ["cols"] ""
            >>= \cols -> do
                  let readN = readMaybe :: String -> Maybe Int
                      mLines = readN $ init lines'
                      mCols = readN $ init cols
                      lines'' = fromMaybe defaultLines mLines
                      cols' = fromMaybe defaultCols mCols

                  when (null mLines || last lines' /= '\n')
                    (proceedError $ "tput failed to produce lines! Defaulting to " <> show defaultLines <> ". Press any key to continue")
                  when (null mCols || last cols /= '\n')
                    (proceedError $ "tput failed to produce columns! Defaulting to " <> show defaultCols <> ". Press any key to continue")

                  pure $ ScreenDimensions lines'' cols'
        )
        (\(e :: IOError.IOError) ->
           if IOError.isDoesNotExistError e
           then do
             proceedError $ "tput not found! Defaulting to " <> show defaultLines <> "x" <> show defaultCols <> ". Press any key to continue"
             pure (ScreenDimensions defaultLines defaultCols)
           else hPutStr stderr "exiting" >>
                (Exception.throwIO . IOError.userError) (show e))

      pure res

getCommand :: IO Command
getCommand =
  hSetBuffering stdin NoBuffering
  >> hSetEcho stdin False
  >> hGetChar stdin
  >>= \input ->
    case input of
      ' ' -> pure Continue
      '?' -> pure Help
      'b' -> pure Back
      'r' -> pure Resize
      'q' -> pure Cancel
      _   -> getCommand

clearScreen :: IO ()
clearScreen =
  putStrLn "clearing screen" >>
  BS.putStr "\^[[1J\^[[1;1H"

showHelp :: IO ()
showHelp = do
  clearScreen
  TextIO.putStrLn "Commands: "
  TextIO.putStrLn "\t <spacebar> -> continue to next page/file"
  TextIO.putStrLn "\t <b>        -> go back to previous page/file"
  TextIO.putStrLn "\t <q>        -> exit -- will exit the help screen if on it, otherwise exits the program"
  TextIO.putStrLn "\t <r>        -> resize terminal window"
  TextIO.putStrLn "\t <?>        -> display this screen"
  TextIO.putStrLn "\n NOTE: going past the last page of the last file or before the first page of the first file closes the application"
  cmd <- getCommand
  unless (cmd == Cancel)
    showHelp

splicePages :: Text.Text -> Text.Text -> Text.Text -> ScreenDimensions -> Text.Text
splicePages firstWord firstPage secondPage (ScreenDimensions rows cols) = do
  let (firstLineFirstPage:restLinesFirstPage) = dropWhile (notElem firstWord . Text.words) $ Text.lines firstPage
      firstLineFirstPage' = Text.unwords $ dropWhile (/= firstWord) (Text.words firstLineFirstPage)
      firstPageLines = firstLineFirstPage' : restLinesFirstPage
      secondPageLines = take (rows - length firstPageLines) $ wordWrap cols secondPage
  Text.unlines $ firstPageLines <> secondPageLines

showPages :: Int -> Int -> [Text.Text] -> IO (Command, Maybe Progress)
showPages _ _ [] = pure (Cancel, Nothing)
showPages charIdx pageIdx pages = do
  let curPage   = pages !! pageIdx
      hasPrev   = pageIdx > 0
      hasNext   = pageIdx < pred (length pages)
      firstWord = head $ Text.words curPage
  clearScreen
  TextIO.putStr curPage
  cmd <- getCommand
  case cmd of
    Help     ->
      showHelp >> showPages charIdx pageIdx pages
    Resize   ->
      pure (Resize, Just Progress { firstWordIdx = charIdx, firstWord, curPage = pageIdx })
    Back     ->
      if hasPrev then
        showPages (charIdx - Text.length (pages !! pred pageIdx)) (pred pageIdx) pages
      else pure (Back, Nothing)
    Continue ->
      if hasNext then
        showPages (charIdx + Text.length curPage) (succ pageIdx) pages
      else pure (Continue, Nothing)
    Cancel   ->
      pure (Cancel, Nothing)

handleArgs :: IO (Either String [FilePath])
handleArgs =
  parseArgs <$> Env.getArgs
  where
    parseArgs argumentList =
      case argumentList of
        []      -> Left "no filename provided"
        fnames  -> Right fnames

-- Given a `charPos` return new page position it's in
findPaginatedFileIdx :: Int -> [Text.Text] -> Int
findPaginatedFileIdx charPos pages = do
  let pageLens = map Text.length pages
      (pageIdx, totalCharPos) =
        foldr (\c (pageIdx', acc) -> if charPos < acc then (succ pageIdx', acc+c) else (pageIdx', acc))
          (0, 0)
          pageLens
      isFirstWordOfPage = totalCharPos == charPos
      isLastPage = pageIdx == pred (length pages)
  if isFirstWordOfPage || isLastPage then pageIdx else succ pageIdx

runHCat :: Maybe Progress -> Int -> [FilePath] -> IO ()
runHCat mProgress fileIdx filePaths = do
  let curFilePath = filePaths !! fileIdx
  contents <- TextIO.hGetContents =<< openFile curFilePath ReadMode
  termSize <- getTerminalSize
  hSetBuffering stdout NoBuffering
  fInfo <- fileInfo curFilePath
  let pages :: [Text.Text] = paginate fileIdx (length filePaths) termSize fInfo contents
  (cmd, progress) <- case mProgress of
                       Nothing             -> showPages 0 0 pages
                       (Just Progress{..}) -> showPages firstWordIdx curPage pages
  case cmd of
    Continue -> unless (fileIdx == pred (length filePaths)) $ runHCat Nothing (succ fileIdx) filePaths
    Resize   -> runHCat progress fileIdx filePaths
    Back     -> unless (fileIdx == 0) $ runHCat Nothing (pred fileIdx) filePaths
    _        -> pure ()

main :: IO ()
main = do
  args <- handleArgs
  targetFilePaths <- eitherToErr args
  runHCat Nothing 0 targetFilePaths
