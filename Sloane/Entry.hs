{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module Sloane.Entry
    ( PackedPrg (..)
    , PackedEntry (..)
    , parsePackedEntry
    , parsePackedEntryErr
    ) where

import GHC.Generics (Generic)
import Data.Maybe
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString.Char8 as Ch
import Data.Attoparsec.ByteString.Char8
import Control.Monad
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Sloane.Utils
import Sloane.OEIS

-- | A compact `ByteString` representation of a `Prg`.
newtype PackedPrg = PPrg ByteString deriving (Eq, Show, Generic)

-- | Similary, a packed entry consists of a packed program together with
-- a packed sequence.
data PackedEntry = PackedEntry
    { getPackedPrg :: PackedPrg
    , getPackedSeq :: PackedSeq
    } deriving (Eq, Show, Generic)

packedEntry :: Parser PackedEntry
packedEntry =
    let f w = if B.last w == '=' then return (PPrg (B.init w)) else mzero
    in PackedEntry <$> (Ch.takeWhile1 (/='>') >>= f)
                   <*> (char '>' *> packedSeq)

-- | A parser for packed entries.
parsePackedEntry :: ByteString -> Maybe PackedEntry
parsePackedEntry = parse_ packedEntry . B.filter (/=' ')

-- | Like `parsePackedEntry` but throws an error rather than returning
-- `Nothing` in case the parse fails.
parsePackedEntryErr :: ByteString -> PackedEntry
parsePackedEntryErr = fromMaybe (error "cannot parse input") . parsePackedEntry
