module Main where

import Data.Maybe
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
import Lib (someFunc)
import Math.GaussianQuadratureIntegration (nIntegrate256)
import Numeric.LinearAlgebra (flatten, fromLists, linearSolve, luSolve, toList, toLists, (><))
import System.Environment (getArgs)
import System.Exit ()

integrate :: (Fractional a, Ord a) => (a -> a) -> Bool -> a
integrate f shouldIntegrate
  | shouldIntegrate = nIntegrate256 f 0 2
  | otherwise = 0

-- basisF docs
basisFWithDomain :: (Ord a, Fractional a) => Int -> Int -> a -> a
basisFWithDomain n i x
  | x < left || right < x = 0
  | x <= center = (x - left) * hInverse
  | otherwise = (right - x) * hInverse
  where
    domainEnd = 2
    left = center - h
    right = center + h
    center = domainEnd * fromIntegral i / fromIntegral n
    h = domainEnd / fromIntegral n
    hInverse = fromIntegral n / domainEnd

basisF'WithDomain :: (Ord a, Fractional a) => Int -> Int -> a -> a
basisF'WithDomain n i x
  | x < left || right < x = 0
  | x <= center = hInverse
  | otherwise = - hInverse
  where
    left = center - h
    right = center + h
    domainEnd = 2
    center = domainEnd * fromIntegral i / fromIntegral n
    h = domainEnd / fromIntegral n
    hInverse = fromIntegral n / domainEnd

-- generateBMatrix :: (Int -> Int -> a) -> Int -> [[a]]
generateBMatrix :: Int -> [[Double]]
generateBMatrix n =
  [[b_ei_ej i j | j <- [1 .. n]] | i <- [1 .. n]]
  where
    -- B(u,v) = u(2)v(2) - int[0, 2] u'v'dx + int[0, 2] uvdx
    -- B(e_i, e_j) = e_i(2)*e_j(2) - int[0, 2] e_i'*e_j'dx + int[0, 2] e_i*e_jdx
    b_ei_ej i j = u2v2 i j + ʃsub_u'v'_add_uvdx i j
    u2v2 i j = basisF i 2 * basisF j 2
    ʃsub_u'v'_add_uvdx i j = integrate (\x -> - basisF' i x * basisF' j x + basisF i x * basisF j x) $ near i j

    near i j = abs (i - j) <= 1
    basisF = basisFWithDomain n
    basisF' = basisF'WithDomain n

-- max(0, min(x_i, x_j) - h) do min(2, max(x_i, x_j) + h)

-- generateLMatrix :: (Ord a, Num a) => Int -> [a]
generateLMatrix :: Int -> [Double]
generateLMatrix n =
  [l_ej j | j <- [1 .. n]]
  where
    -- L(v) = - int[0, 2] v*sindx
    -- L(e_j) = - int[0, 2] e_j*sindx
    l_ej j = - integrate (\x -> basisF j x * sin x) (j > 0)
    basisF = basisFWithDomain n

parseArg :: [String] -> Int
parseArg [arg] = read arg :: Int
parseArg _ = 5

plotData c_vector n = [(x, u x) | x <- plotPoints] :: [(Double, Double)]
  where
    plotPoints = [0, (0.1) .. 2]
    u x = sum $ [c_i * basisF i x | (i, c_i) <- enumerate c_vector]
    enumerate = zip [0 ..]
    basisF = basisFWithDomain n

main :: IO ()
main = do
  args <- getArgs
  let n = parseArg args

  let b_matrix = fromLists $ generateBMatrix n
  let l_vector = (n >< 1) $ generateLMatrix n
  let c_vector = toList $ flatten (fromJust $ linearSolve b_matrix l_vector)

  print "B matrix"
  print b_matrix
  print "L vector"
  print l_vector
  print "c vector"
  print c_vector

  toFile def "mychart.svg" $ do
    layout_title .= "Data"
    setColors [opaque blue, opaque red]
    plot (line "" [plotData c_vector n])

  putStrLn "Done."
