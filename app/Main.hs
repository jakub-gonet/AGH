module Main where

import Data.Maybe
import Lib (someFunc)
import Math.GaussianQuadratureIntegration (nIntegrate512)
import Numeric.LinearAlgebra (fromLists, linearSolve, (><))
import System.Environment (getArgs)
import System.Exit ()

integrate :: (Fractional a, Ord a) => (a -> a) -> a -> a -> a
integrate f i j
  | abs (i - j) <= 1 = nIntegrate512 f i j
  | otherwise = 0

-- basisF docs
basisFWithDomain :: (Ord a, Fractional a) => Int -> Int -> a -> a
basisFWithDomain n i x
  | x < left || x > right = 0
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
  | x < left || x > right = 0
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
  [[b_ei_ej i j | j <- [0 .. n -1]] | i <- [0 .. n -1]]
  where
    -- B(u,v) = u(2)v(2) - int[0, 2] u'v'dx + int[0, 2] uvdx
    -- B(e_i, e_j) = e_i(2)*e_j(2) - int[0, 2] e_i'*e_j'dx + int[0, 2] e_i*e_jdx
    b_ei_ej i j = u2v2 i j - int_u'_v' i j + int_u_v i j
    u2v2 i j = basisF i 2 * basisF j 2
    int_u'_v' i j = integrate (\x -> basisF' i x * basisF' j x) 0 2
    int_u_v i j = integrate (\x -> basisF i x * basisF j x) 0 2

    basisF = basisFWithDomain n
    basisF' = basisF'WithDomain n

-- generateLMatrix :: (Ord a, Num a) => Int -> [a]
generateLMatrix :: Int -> [Double]
generateLMatrix n =
  [l_ej j | j <- [0 .. n -1]]
  where
    -- L(v) = - int[0, 2] v*sindx
    -- L(e_j) = - int[0, 2] e_j*sindx
    l_ej j = - integrate (\x -> basisF j x * sin x) 0 2
    basisF = basisFWithDomain n

parseArg :: [String] -> Int
parseArg [arg] = read arg :: Int
parseArg _ = 5

main :: IO ()
main = do
  args <- getArgs
  let n = parseArg args

  let b_matrix = fromLists $ generateBMatrix n
  let l_matrix = (n >< 1) $ generateLMatrix n
  -- print $ [basisFWithDomain n i 2 | i <- [1 .. 10]]
  print b_matrix
  print l_matrix

  putStrLn "Done."
