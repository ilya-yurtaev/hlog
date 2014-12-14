module Hlog where

import Data.Maybe
import Data.Time (formatTime, getZonedTime)
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
  time <- getZonedTime
  return $ formatTime defaultTimeLocale dtFmt time


hlog :: String -> IO ()
hlog msg = do
  ts <- timestamp
  fn <- logFile
  appendFile fn (printf msgFmt ts msg)
