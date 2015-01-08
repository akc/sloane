-- |
-- Copyright   : Anders Claesson 2012-2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
module Sloane.Transform
    ( NamedTransform (..)
    , ($$)
    , tLEFT
    , tRIGHT
    , tM2
    , tM2i
    , tBINOMIAL
    , tBINOMIALi
    , tBIN1
    , tBISECT0
    , tBISECT1
    , tCONV
    , tCONVi
    , tEXPCONV
    , tDIFF
    , tMOBIUS
    , tMOBIUSi
    , tEULER
    , tEXP
    , tLOG
    , tNEGATE
    , tPRODS
    , tPSUM
    , tPSUMSIGN
    , tSTIRLING
    , tTRISECT0
    , tTRISECT1
    , tTRISECT2
    , tPOINT
    , tWEIGHT
    , tPARTITION
    , transforms
    , lookupTranform
    , applyAllTransforms
    ) where

import Data.List
import Data.Ratio
import Data.Monoid
import Control.Monad
import Sloane.GF

type Transform = [Rational] -> [Rational]

data NamedTransform = NT
    { name :: String
    , eval :: Transform
    }

instance Show NamedTransform where
    show = ('t':) . name

instance Eq NamedTransform where
    t == s = name t == name s

instance Monoid NamedTransform where
    mempty = NT "" id
    mappend f g = NT tname teval
      where
        tname = name f ++ "." ++ name g
        teval = eval f     .     eval g

infixr 0 $$

isInteger :: Rational -> Bool
isInteger = (1==) . denominator

toIntSeq :: [Rational] -> [Integer]
toIntSeq cs = [ numerator c | c <- cs, all isInteger cs ]

($$) :: NamedTransform -> [Integer] -> [Integer]
f $$ cs = toIntSeq $ take (length cs) (eval f (map toRational cs))

x :: GF
x = ogf [0::Integer, 1]

-- addSeq :: [Integer] -> Transform
-- addSeq seq0 = zipWith (+) seq0

-- mulOGF :: [Integer] -> Transform
-- mulOGF seq0 = \cs -> ogfCoeffs (ogf seq0 * ogf cs)

-- mulEGF :: [Integer] -> Transform
-- mulEGF seq0 = \cs -> egfCoeffs (egf seq0 * egf cs)

geoSeries :: Rational -> GF
geoSeries c = ogf [ c^k | k<-[0::Int ..] ]

expSeries :: Rational -> GF
expSeries c = egf [ c^k | k<-[0::Int ..] ]

bisect0 :: [a] -> [a]
bisect0 [] = []
bisect0 (c:cs) = c : bisect1 cs

bisect1 :: [a] -> [a]
bisect1 [] = []
bisect1 (_:cs) = bisect0 cs

trisect0 :: [a] -> [a]
trisect0 [] = []
trisect0 (c:cs) = c : trisect2 cs

trisect1 :: [a] -> [a]
trisect1 [] = []
trisect1 (_:cs) = trisect0 cs

trisect2 :: [a] -> [a]
trisect2 [] = []
trisect2 (_:cs) = trisect1 cs

signed :: GF -> GF
signed = imap $ \i c -> (-1%1)^i * c

tLEFT :: NamedTransform
tLEFT = NT "LEFT" (drop 1)

tRIGHT :: NamedTransform
tRIGHT = NT "RIGHT" (1:)

tM2 :: NamedTransform
tM2 = NT "M2" f where f [] = []; f (c:cs) = c : map ((2%1)*) cs

tM2i :: NamedTransform
tM2i = NT "M2i" (ogfCoeffs . Series . f)
  where
    f [] = []
    f cs = let (d:ds) = map toRational cs in d : map (/(2%1)) ds

tBINOMIAL :: NamedTransform
tBINOMIAL = NT "BINOMIAL" (\cs -> egfCoeffs (expSeries (1%1) * egf cs))

tBINOMIALi :: NamedTransform
tBINOMIALi = NT "BINOMIALi" (\cs -> egfCoeffs (expSeries ((-1)%1) * egf cs))

tBIN1 :: NamedTransform
tBIN1 = NT "BIN1" (\cs ->
    drop 1 . egfCoeffs $ -expSeries (-1%1) * signed (egf (0:cs)))

tBISECT0 :: NamedTransform
tBISECT0 = NT "BISECT0" bisect0

tBISECT1 :: NamedTransform
tBISECT1 = NT "BISECT1" bisect1

tCONV :: NamedTransform
tCONV = NT "CONV" (\cs -> ogfCoeffs (ogf cs ^ (2::Int)))

tCONVi :: NamedTransform
tCONVi = NT "CONVi" (ogfCoeffs . squareRoot . ogf)

tEXPCONV :: NamedTransform
tEXPCONV = NT "EXPCONV" (\cs -> egfCoeffs (egf cs ^ (2::Int)))

tDIFF :: NamedTransform
tDIFF = NT "DIFF" (\cs -> zipWith (-) (drop 1 cs) cs)

-- The Mobius function of the poset of integers under divisibility
mobius :: Integer -> Integer -> Integer
mobius a b
  | a == b         = 1
  | b `rem` a == 0 = -sum [ mobius a c | c <- [a..b-1], b `rem` c == 0 ]
  | otherwise      = 0

-- The number theoretical Mobius function
mu :: Integer -> Integer
mu = mobius 1

tMOBIUS :: NamedTransform
tMOBIUS = NT "MOBIUS" $ \cs ->
    [ sum [mu (n `div` k) % 1 * (cs !! (fromInteger k-1))
          | k<-[1..n], n `rem` k == 0
          ]
    | (n,_) <- zip [1..] cs
    ]

tMOBIUSi :: NamedTransform
tMOBIUSi = NT "MOBIUSi" $ \cs ->
    [ sum [ cs !! (fromInteger k-1) | k<-[1..n], n `rem` k == 0 ]
    | (n,_) <- zip [1..] cs
    ]

tEULER :: NamedTransform
tEULER = NT "EULER" (\cs ->
    let f = product $ zipWith (\n c -> (1 - x^n)^^^c) [1::Int ..] cs
    in drop 1 $ ogfCoeffs (1/f))

tEULERi :: NamedTransform
tEULERi = NT "EULERi" undefined

-- EXP converts [a_1, a_2, ...] to [b_1, b_2,...] where
-- 1 + EGF_B (x) = exp EGF_A (x)
tEXP :: NamedTransform
tEXP = NT "EXP" (\cs -> drop 1 . egfCoeffs $ expSeries 1 `o` egf (0:cs))

-- LOG converts [a_1, a_2, ...] to [b_1, b_2,...] where
-- 1 + EGF_A (x) = exp EGF_B (x) i.e. EGF_B (x) = log(1 + EGF_A (x)).
tLOG :: NamedTransform
tLOG = NT "LOG" (\cs -> drop 1 $ egfCoeffs (log1 `o` (-1 * egf (0:cs))))
  where
    log1 = Series (0 : [-1 % n | n <- [1..]])

tNEGATE :: NamedTransform
tNEGATE = NT "NEGATE" f where f [] = []; f (c:cs) = c : map negate cs

tPRODS :: NamedTransform
tPRODS = NT "PRODS" (drop 1 . scanl (*) (1%1))

tPSUM :: NamedTransform
tPSUM = NT "PSUM" (drop 1 . scanl (+) (0%1))

tPSUMSIGN :: NamedTransform
tPSUMSIGN = NT "PSUMSIGN" (ogfCoeffs . (geoSeries (-1%1) *) . ogf)

tSTIRLING :: NamedTransform
tSTIRLING = NT "STIRLING" (\cs ->
    drop 1 . egfCoeffs $ egf (0:cs) `o` (expSeries (1%1) - 1))

tTRISECT0 :: NamedTransform
tTRISECT0 = NT "TRISECT0" trisect0

tTRISECT1 :: NamedTransform
tTRISECT1 = NT "TRISECT1" trisect1

tTRISECT2 :: NamedTransform
tTRISECT2 = NT "TRISECT2" trisect2

tPOINT :: NamedTransform
tPOINT = NT "POINT" (zipWith (*) [0..])

tWEIGHT :: NamedTransform
tWEIGHT = NT "WEIGHT" $
    drop 1 . ogfCoeffs . product . zipWith (\n c -> (1 + x^n)^^^c) [1::Int ..]

increasing :: Ord a => [a] -> Bool
increasing cs = and $ zipWith (<=) cs (drop 1 cs)

tPARTITION :: NamedTransform
tPARTITION = NT "PARTITION" $ \cs -> do
    guard $ not (null cs) && all (>0) cs && increasing cs
    let f = product $ map (\c -> 1 - x^^^c) (nub cs)
    drop 1 . ogfCoeffs $ 1/f

-- New transform: tSTIELTJES -- continued fraction coefficients


transforms :: [NamedTransform]
transforms =
    [ tLEFT
    , tRIGHT
    , tM2
    , tM2i
    , tBINOMIAL
    , tBINOMIALi
    , tBIN1
    , tBISECT0
    , tBISECT1
    , tCONV
    , tCONVi
    , tEXPCONV
    , tDIFF
    , tMOBIUS
    , tMOBIUSi
    , tEULER
--    , tEULERi
    , tEXP
    , tLOG
    , tNEGATE
    , tPRODS
    , tPSUM
    , tPSUMSIGN
    , tSTIRLING
    , tTRISECT0
    , tTRISECT1
    , tTRISECT2
    , tPOINT
    , tWEIGHT
    , tPARTITION
    ]

lookupTranform :: String -> Maybe NamedTransform
lookupTranform tname = lookup tname [ (name f, f) | f <- transforms ]

applyAllTransforms :: [Rational] -> [(NamedTransform, [Rational])]
applyAllTransforms cs = [ (f, eval f cs) | f <- transforms ]
