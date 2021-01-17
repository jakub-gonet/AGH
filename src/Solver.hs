module Solver (coefficients, approxU, getLMatrix, getBMatrix) where

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
    l_ej j = - integrate (\x -> basisF j x * sin x) (j > 0)
    basisF = basisFWithDomain n

getBMatrix :: Int -> [[Double]]
getBMatrix n =
  [[b_ei_ej i j | j <- [1 .. n]] | i <- [1 .. n]]
  where
    -- B(u,v) = u(2)v(2) + int[0, 2] (-u'v' + uv) dx
    -- B(e_i, e_j) = e_i(2)*e_j(2) + int[0, 2] (- e_i'*e_j' + e_i*e_j) dx
    b_ei_ej i j = u2v2 i j + ʃsub_u'v'_add_uvdx i j
    u2v2 i j = basisF i 2 * basisF j 2
    ʃsub_u'v'_add_uvdx i j = integrate (\x -> - basisF' i x * basisF' j x + basisF i x * basisF j x) $ near i j
    near i j = abs (i - j) <= 1
    basisF = basisFWithDomain n
    basisF' = basisF'WithDomain n

integrate :: (Fractional a, Ord a) => (a -> a) -> Bool -> a
integrate f shouldIntegrate
  | shouldIntegrate = nIntegrate256 f 0 2
  | otherwise = 0

-- max(0, min(x_i, x_j) - h) do min(2, max(x_i, x_j) + h)

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
