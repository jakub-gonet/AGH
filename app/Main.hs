module Main where

import Lib (someFunc)
import System.Environment (getArgs)
import System.Exit ()

main :: IO ()
main = do
  args <- getArgs
  let n = read $ head args :: Int

  putStrLn "Done."
