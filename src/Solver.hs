module Solver (coefficients, approxU, getLMatrix, getBMatrix, domainStart, domainEnd) where

import Data.Maybe (fromJust)
import Math.GaussianQuadratureIntegration (nIntegrate256)
import Numeric.LinearAlgebra (flatten, fromLists, linearSolve, luSolve, toList, toLists, (><))

approxU :: Int -> Double -> Double
approxU n x =
  sum . zipWith (\i c_i -> c_i * basisF i x) [0 ..] . coefficients $ n
  where
    basisF = basisFWithDomain n

coefficients :: Int -> [Double]
coefficients n = 0 : (toList . flatten $ fromJust (linearSolve b_matrix l_vector))
  where
    b_matrix = fromLists $ getBMatrix n
    l_vector = (n >< 1) $ getLMatrix n

getLMatrix :: Int -> [Double]
getLMatrix n =
  [l_ej j | j <- [1 .. n]]
  where
    -- L(v) = - int[0, 2] v*sindx
    -- L(e_j) = - int[0, 2] e_j*sindx
    -- u(0) => e_j = 0, so we skip it
    l_ej j = - integrate domainStart domainEnd (j > 0) (\x -> basisF j x * sin x)
    basisF = basisFWithDomain n

getBMatrix :: Int -> [[Double]]
getBMatrix n =
  [[b_ei_ej i j | j <- [1 .. n]] | i <- [1 .. n]]
  where
    -- B(u,v) = u(2)v(2) + int[0, 2] (-u'v' + uv) dx
    -- B(e_i, e_j) = e_i(2)*e_j(2) + int[0, 2] (- e_i'*e_j' + e_i*e_j) dx
    b_ei_ej i j = u2v2 i j + ʃsub_u'v'_add_uvdx i j
    u2v2 i j = basisF i domainEnd * basisF j domainEnd
    -- we only need to integrate splines with non-zero intersection
    ʃsub_u'v'_add_uvdx i j = integrate lowerBound upperBound (near i j) (\x -> - basisF' i x * basisF' j x + basisF i x * basisF j x)
      where
        -- finding intersection of tent functions
        lowerBound = dx * max (bigger_idx - 1) domainStart
        upperBound = dx * min (smaller_idx + 1) (fromIntegral n)
        dx = domainEnd / fromIntegral n
        smaller_idx = min (fromIntegral i) (fromIntegral j)
        bigger_idx = max (fromIntegral i) (fromIntegral j)
    near i j = abs (i - j) <= 1
    basisF = basisFWithDomain n
    basisF' = basisF'WithDomain n

integrate :: (Fractional a, Ord a) => a -> a -> Bool -> (a -> a) -> a
integrate start end nonZeroCond f
  | nonZeroCond = nIntegrate256 f start end
  | otherwise = 0

-- tent function used for generating vector space
-- https://en.wikipedia.org/wiki/Triangular_function
basisFWithDomain :: Int -> Int -> Double -> Double
basisFWithDomain n i x
  | x < left || right < x = 0
  | x <= center = (x - left) * hInverse
  | otherwise = (right - x) * hInverse
  where
    left = center - h
    right = center + h
    center = domainEnd * fromIntegral i / fromIntegral n
    h = domainEnd / fromIntegral n
    hInverse = fromIntegral n / domainEnd

-- derivative of tent function
basisF'WithDomain :: Int -> Int -> Double -> Double
basisF'WithDomain n i x
  | x < left || right < x = 0
  | x <= center = hInverse
  | otherwise = - hInverse
  where
    left = center - h
    right = center + h
    center = domainEnd * fromIntegral i / fromIntegral n
    h = domainEnd / fromIntegral n
    hInverse = fromIntegral n / domainEnd

domainEnd :: Double
domainEnd = 2

domainStart :: Double
domainStart = 0