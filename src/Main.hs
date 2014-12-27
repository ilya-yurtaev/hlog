module Main where

import System.Environment (getArgs)
import Hlog (hlog, showTodayEntries, undo)


main :: IO ()
main = do
  args <- getArgs
  case args of
       [] -> do
         putStrLn "No message supplied"
         showTodayEntries

       ["today"] -> showTodayEntries

       ["undo"] -> do
         undo
         showTodayEntries

       _  -> hlog $ unwords args
