-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module Sloane.Bloom
    ( mkBloomFilter
    , isFactorOf
    ) where

import Data.ByteString.Char8 (ByteString)
import Data.BloomFilter (Bloom)
import qualified Data.BloomFilter as F
import Data.BloomFilter.Hash
import Sloane.OEIS
import Sloane.DB

newtype T9 = T9 ( ByteString, ByteString, ByteString
                , ByteString, ByteString, ByteString
                , ByteString, ByteString, ByteString )

type BlF = Bloom T9

instance Hashable T9 where
    hashIO32 (T9 (a,b,c,d,e,f,g,h,i)) salt =
        hashIO32 a salt >>= hashIO32 b >>= hashIO32 c >>= hashIO32 d >>= hashIO32 e
                        >>= hashIO32 f >>= hashIO32 g >>= hashIO32 h >>= hashIO32 i

zip9 :: [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i] -> [(a,b,c,d,e,f,g,h,i)]
zip9 (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) (i:is)
                       = (a,b,c,d,e,f,g,h,i) : zip9 as bs cs ds es fs gs hs is
zip9 _ _ _ _ _ _ _ _ _ = []

ninegrams :: [ByteString] -> [T9]
ninegrams xs = map T9 $ zip9 xs (drop 1 xs) (drop 2 xs) (drop 3 xs) (drop 4 xs)
                                (drop 5 xs) (drop 6 xs) (drop 7 xs) (drop 8 xs)

-- | Make a Bloom filter of all nine integer segments of all sequences
-- in the given data base.
mkBloomFilter :: DB Sequences -> BlF
mkBloomFilter (DB db) = F.fromList (cheapHashes numHashes) numBits ts
  where
    ts = ninegrams (parseTermsOfRecords db)
    numHashes = 3
    numBits = 2^(26 :: Int)

-- | Are all the nine element factors of the given (packed) sequence
-- members of the Bloom filter. May give a false positive answer, but
-- never a false negative answer.
isFactorOf :: PackedSeq -> BlF -> Bool
isFactorOf (PSeq s) bf = all (`F.elem` bf) $ ninegrams (parseTermsErr s)
