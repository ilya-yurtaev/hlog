module Main where

import System.Environment (getArgs, getEnv)
import Hlog (hlog)


main :: IO ()
main = do
  args <- getArgs
  case args of
       [] -> error "No message supplied"
       _  -> hlog $ unwords args
