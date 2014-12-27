module Hlog where

import Control.Monad
import Control.Applicative
import Data.Map (fromList, lookup)
import Data.List (delete)
import Data.List.Utils (startswith)
import Data.Time (formatTime, getZonedTime)
import Data.String.Utils (split)
import System.Directory (renameFile)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.Locale (defaultTimeLocale)
import Text.Printf


delimiter = " | "

dateFmt = "%Y-%m-%d"

timeFmt = "%H:%M:%S"

dateTimeFmt = unwords [dateFmt, timeFmt]


showHelp :: IO ()
showHelp = print "help"


timeRepr :: String -> IO String
timeRepr fmtStr = do
  time <- getZonedTime
  return $ formatTime defaultTimeLocale fmtStr time


absPath :: String -> IO FilePath
absPath fn = do
  homeDir <- getEnv "HOME"
  return $ homeDir </> fn


logFile :: IO FilePath
logFile = absPath ".hlog"


tmpFile :: IO FilePath
tmpFile = absPath ".hlog.tmp"


mkentry :: String -> IO String
mkentry msg = do
  ts <- mktimestamp
  return $ printf "%s%s%s\n" ts delimiter msg


mktimestamp :: IO String
mktimestamp = timeRepr dateTimeFmt


undo :: IO ()
undo = do
  -- deletes last entry
  entries <- getEntries
  fn <- logFile
  saveEntries $ init entries


stripDateTime :: String -> String
stripDateTime entry = last $ split delimiter entry


stripDate :: String -> String
stripDate entry = unwords $ tail $ words entry


enumerate :: [String] -> String
enumerate entries = unlines $ map addNum $ zip (map show [1..]) (map stripDate entries)
  where addNum (x, y) = printf "%s. %s" x y


todayEntries :: IO [String]
todayEntries = do
  entries <- getEntries
  filterM isToday entries


showTodayEntries :: IO ()
showTodayEntries = do
  entries <- todayEntries
  putStrLn $ case entries of [] -> "No entries for today"
                             _  -> enumerate entries


deleteEntry index = do
  entries <- todayEntries
  saveEntries $ delete (entries !! index) entries


isToday :: String -> IO Bool
isToday x = do
  td <- timeRepr dateFmt
  return $ startswith td x


getEntries :: IO [String]
getEntries = filter (not . null) <$> lines <$> (readFile =<< logFile)


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
