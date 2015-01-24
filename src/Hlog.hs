module Hlog
  ( hlog
  , showLatest
  , showTodayEntries
  , undo
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Map (fromList, lookup)
import Data.List (delete)
import Data.List.Utils (startswith)
import Data.Time (formatTime, getZonedTime)
import Data.String.Utils (split)
import System.Directory (renameFile)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)


delimiter = " | "

dateFmt = "%Y-%m-%d"

timeFmt = "%H:%M:%S"

entryFmt = "%s%s%s\n" -- timestamp delimiter message

dateTimeFmt = unwords [dateFmt, timeFmt]


absPath :: String -> IO FilePath
absPath fn = return <$> (\x -> x </> fn) =<< (getEnv "HOME")


logFile :: IO FilePath
logFile = absPath ".hlog"


tmpFile :: IO FilePath
tmpFile = absPath ".hlog.tmp"


timeRepr :: String -> IO String
timeRepr fmtStr = return <$> formatTime defaultTimeLocale fmtStr =<< getZonedTime


mkentry :: String -> IO String
mkentry msg = do
  timestamp <- mktimestamp
  return $ printf entryFmt timestamp delimiter msg


mktimestamp :: IO String
mktimestamp = timeRepr dateTimeFmt


undo :: IO ()
undo = (init <$> getEntries) >>= saveEntries


stripDateTime :: String -> String
stripDateTime = last . (split delimiter)


stripDate :: String -> String
stripDate = (unwords . tail . words)


enumerate :: [String] -> String
enumerate entries = unlines $ map addNum $ zip (map show [1..]) (map stripDate entries)
  where addNum (x, y) = printf "%s. %s" x y


todayEntries :: IO [String]
todayEntries = do
  td <- timeRepr dateFmt
  entries <- getEntries
  return $ filter (startswith td) entries


showTodayEntries :: IO ()
showTodayEntries = do
  entries <- todayEntries
  putStrLn $ case entries of
    [] -> "No entries for today"
    _  -> enumerate entries


deleteEntry :: Int -> IO ()
deleteEntry index = saveEntries <$> (\es -> delete (es !! index) es) =<< todayEntries


getEntries :: IO [String]
getEntries = filter (not . null) <$> lines <$> (readFile =<< logFile)


showLatest :: IO ()
showLatest = do
  entries <- getEntries
  putStrLn $ case entries of
    [] -> "No entries at all"
    _  -> unlines $ (reverse . (take 10) . reverse) entries


saveEntries :: [String] -> IO ()
saveEntries entries = do
  fn <- logFile
  tmp <- tmpFile
  writeFile tmp (unlines entries)
  renameFile tmp fn


hlog :: String -> IO ()
hlog msg = do
  fn <- logFile
  entry <- mkentry msg
  appendFile fn entry
