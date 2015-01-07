-- |
-- Copyright   : Anders Claesson 2012-2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
module Sloane.GF
    ( Series (..)
    , GF
    , imap
    , diff
    , squareRoot
    , (^^^)
    , o
    , ogf
    , egf
    , ogfCoeffs
    , egfCoeffs
    )where

import Data.List
import Data.Maybe
import Data.Ratio

-- Reference: M. D. McIlroy, The music of streams,
-- Information Processing Letters 77 (2001) 189-195.

newtype Series a = Series { coeffs :: [a] } deriving (Show, Eq)

type GF = Series Rational

instance Functor Series where
    fmap f = Series . fmap f . coeffs

instance (Eq a, Num a) => Num (Series a) where
    (+) = lift2 add
    (*) = lift2 mul
    fromInteger c = Series [fromInteger c]
    negate = fmap negate
    signum = undefined
    abs    = undefined

instance (Eq a, Fractional a) => Fractional (Series a) where
     fromRational c = Series [fromRational c]
     (/) = lift2 divide

degree :: (Num a, Eq a) => Series a -> Int
degree (Series [0]) = -1
degree (Series cs)  = length cs - 1
-- degree (Series cs)  = length (dropWhile (==0) (reverse cs)) - 1

o :: (Eq a, Fractional a) => Series a -> Series a -> Series a
o = lift2 compose

lift :: ([a] -> [b]) -> Series a -> Series b
lift f (Series as) = Series (f as)

lift2 :: ([a] -> [b] -> [c]) -> Series a -> Series b -> Series c
lift2 op (Series as) (Series bs) = Series (as `op` bs)

add :: Num a => [a] -> [a] -> [a]
add []     ds     = ds
add cs     []     = cs
add (c:cs) (d:ds) = c+d : add cs ds

sub :: Num a => [a] -> [a] -> [a]
cs `sub` ds = cs `add` map negate ds

(!*!) :: (Eq a, Num a) => a -> a -> a
(!*!) _ 0 = 0
(!*!) 0 _ = 0
(!*!) a b = a*b

mul :: (Eq a, Num a) => [a] -> [a] -> [a]
mul (c:ct) ds@(d:dt) = c!*!d : map (c !*!) dt `add` (ct `mul` ds)
mul _      _         = []

divide :: (Eq a, Fractional a) => [a] -> [a] -> [a]
divide []     (0:_)     = undefined
divide []      _        = []
divide (0:ct) (0:dt)    = ct `divide` dt
divide (c:ct) ds@(d:dt) = q : (ct `sub` ([q] `mul` dt)) `divide` ds where q = c/d
divide _      []        = undefined

diff :: GF -> GF
diff = lift dF where {dF [] = []; dF (_:ct) = zipWith (*) [1..] ct}

nthRootApprox :: Integer -> GF -> [GF]
nthRootApprox n f@(Series (1:_)) =
    iterateUntilFixed (nthRootNext n f) (Series [1])
nthRootApprox _ _ = error "GF has constant term different from 1"

nthRootNext :: Integer -> GF -> GF -> GF
nthRootNext n f g = Series (take (1 + 2*degree f) ds)
  where
    Series ds = (Series [n%1-1] * g^n + f) / (Series [n%1] * g^(n-1))

iterateUntilFixed :: Eq a => (a -> a) -> a -> [a]
iterateUntilFixed f x = x : (if x == y then [] else ys)
  where
    ys@(y:_) = iterateUntilFixed f (f x)

saddlePoint :: Eq a => [a] -> Maybe a
saddlePoint [c] = Just c
saddlePoint cs  = listToMaybe [ c | (c,d) <- zip cs (drop 1 cs), c == d ]

nthRoot1 :: Integer -> GF -> GF
nthRoot1 n f@(Series (1:_)) =
    let css = transpose (map coeffs (nthRootApprox n f))
    in Series $ map fromJust (takeWhile isJust (map saddlePoint css))
nthRoot1 _ _ = error "GF has constant term different from 1"

nthRoot :: Integer -> GF -> GF
nthRoot _ (Series [])       = Series []
nthRoot n (Series cs@(c:_)) = Series [d] * nthRoot1 n (Series $ map (/c) cs)
    where
      d = toRational (fromRational c ** fromRational (1%n) :: Double)

(^^^) :: GF -> Rational -> GF
(^^^) f r = case (numerator r, denominator r) of
              (n, 1) -> f ^^ n
              (0, _) -> ogf [1::Int]
              (n, k) -> nthRoot k f ^^ n

-- XXX: Deprecate and use (^^^(1%2)) instead?
squareRoot :: GF -> GF
squareRoot = Series . map toRational . squareRoot' . map fromRational . coeffs

-- XXX: Deprecate?
squareRoot' :: [Double] -> [Double]
squareRoot' []     = []
squareRoot' (c:ct) = ds
  where
    ds = d : ct `divide` ([d] `add` ds)
    d  = sqrt c

compose :: (Eq a, Fractional a) => [a] -> [a] -> [a]
[]     `compose` _         = []
(c:_)  `compose` []        = [c]
(c:ct) `compose` ds@(0:dt) = c : dt `mul` (ct `compose` ds)
(c:ct) `compose` ds        = [c] `add` ds `mul` (ct `compose` ds)
                                              -- ct must be finite

imap :: (Integer -> a -> b) -> Series a -> Series b
imap f = Series . zipWith f [0..] . coeffs

factorial :: Integer -> Integer
factorial n = product [1..n]

ogf :: Real a => [a] -> GF
ogf = Series . map toRational

ogfCoeffs :: GF -> [Rational]
ogfCoeffs = coeffs

egf :: Real a => [a] -> GF
egf = Series . zipWith (\n c -> toRational c * (1 % factorial n)) [0..]

egfCoeffs :: GF -> [Rational]
egfCoeffs = ogfCoeffs . imap (\i -> (* (factorial i % 1)))
