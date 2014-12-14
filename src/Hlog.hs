module Hlog where

import Data.Maybe
import Data.Time (formatTime, getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import System.Environment (getEnv)
import System.Locale (defaultTimeLocale)
import Text.Printf


msgFmt = "\n%s | %s\n"

dtFmt = "%Y-%m-%d %H:%M:%S"


logFile :: IO FilePath
logFile = do
  homeDir <- getEnv "HOME"
  return $ homeDir </> ".hlog"


timestamp :: IO String
timestamp = do
  time <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ formatTime defaultTimeLocale dtFmt (utcToLocalTime tz time)


hlog :: String -> IO ()
hlog msg = do
  ts <- timestamp
  fn <- logFile
  appendFile fn (printf msgFmt ts msg)
