module WordReplacementUtility where

import Prelude
import Control.Monad.Except
import System.Environment (getArgs)
import Data.Foldable (Foldable(foldl'))

newtype Path = Path String
newtype Needle = Needle String
newtype Replacement = Replacement String

parsePath :: String -> Path
parsePath = Path

isWord :: String -> Bool
isWord s = not (null s) && length (words s) == 1

parseNeedle :: String -> Either String Needle
parseNeedle s = if isWord s
  then Right $ Needle s
  else Left $ "Bad needle " <> s <> " provided"

parseReplacement :: String -> Either String Replacement
parseReplacement s = if isWord s
  then Right $ Replacement s
  else Left $ "Bad replacement " <> s <> " provided"

-- Parse and validate command line args
parseArgs :: [String] -> Either String (Path, Needle, Replacement)
parseArgs args =
  if length args /= 3
    then Left "Wrong number of args passed, expected 3"
    else do
      let (arg0:arg1:arg2:_) = args
          path = parsePath arg0
          eNeedle = parseNeedle arg1
          eReplacement = parseReplacement arg2
      case (eNeedle, eReplacement) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right needle, Right replacement) -> Right (path, needle, replacement)

readAndReplace :: Path -> Needle -> Replacement -> IO String
readAndReplace (Path p) (Needle n) (Replacement r) = do
  fileContents <- readFile p
  let fileWords = words fileContents
      replacedWords = map (\word -> if word == n then r else word) fileWords
  pure $ unwords replacedWords

main :: IO ()
main = do
  args <- getArgs
  let eArgs = parseArgs args
  case eArgs of
    Left err -> print err
    Right (path, needle, replacement) -> readAndReplace path needle replacement >>= print
