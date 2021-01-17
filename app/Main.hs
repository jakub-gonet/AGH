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
    points,
    red,
    setColors,
    (.=),
  )
import Solver (coefficients, domainEnd, domainStart)
import System.Environment (getArgs)
import System.Exit ()
import Text.Read (readMaybe)

approxPlot :: Int -> [(Double, Double)]
approxPlot n = [((domainEnd - domainStart) / fromIntegral (length coef - 1) * i, c_i) | (i, c_i) <- zip [0 ..] coef] :: [(Double, Double)]
  where
    coef = coefficients n

exactPlot :: [(Double, Double)]
exactPlot = [(x, u x) | x <- [domainStart, 0.1 .. domainEnd]] :: [(Double, Double)]
  where
    u x = 0.5 * (x * cos x + ((cos 2 + 2 * sin 2) * sin x) / (cos 2 - sin 2))

parseArg :: [String] -> Int
parseArg [arg] = read arg
parseArg _ = 5

main :: IO ()
main = do
  args <- getArgs
  let n = fromMaybe 5 . readMaybe . head $ args :: Int

  toFile def "solution.svg" $ do
    layout_title .= "-u'' - u = sinx        u(0) = 0,    u'(2) - u(2) = 0"
    setColors [opaque blue, opaque blue, opaque red]
    plot (line "Aproximate solution" [approxPlot n])
    plot (points "" $ approxPlot n)
    plot (line "Exact solution" [exactPlot])

  putStrLn "Done."
