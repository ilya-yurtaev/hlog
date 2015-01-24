module Main where

import System.Environment (getArgs)
import Hlog
{-(hlog, showTodayEntries, undo)-}


main :: IO ()
main = do
  args <- getArgs
  case args of
       [] -> do
         putStrLn "No message supplied"
         showLatest

       ["today"] -> showTodayEntries

       ["undo"] -> do
         undo
         showLatest

       _  -> hlog $ unwords args
