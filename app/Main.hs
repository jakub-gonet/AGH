module Main where

import Data.Maybe (fromMaybe)
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy
  ( Default (def),
    blue,
    layout_title,
    line,
    opaque,
    plot,
    red,
    setColors,
    (.=),
  )
import Solver (approxU)
import System.Environment (getArgs)
import System.Exit ()
import Text.Read (readMaybe)

approxPlot :: Int -> [(Double, Double)]
approxPlot n = [(x, u x) | x <- [0, 0.1 .. 2]] :: [(Double, Double)]
  where
    u = approxU n

exactPlot :: [(Double, Double)]
exactPlot = [(x, u x) | x <- [0, 0.1 .. 2]] :: [(Double, Double)]
  where
    u x = 0.5 * (x * cos x + ((cos 2 + 2 * sin 2) * sin x) / (cos 2 - sin 2))

parseArg :: [String] -> Int
parseArg [arg] = read arg
parseArg _ = 5

main :: IO ()
main = do
  args <- getArgs
  let n = fromMaybe 5 . readMaybe . head $ args :: Int

  toFile def "mychart.svg" $ do
    layout_title .= "Data"
    setColors [opaque blue, opaque red]
    plot (line "Aproximate solution" [approxPlot n])
    plot (line "Exact solution" [exactPlot])

  putStrLn "Done."
