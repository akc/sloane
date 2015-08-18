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
import Data.Monoid
import Data.Maybe
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString.Char8 as Ch
import Data.Attoparsec.ByteString.Char8
import Data.Aeson
import Control.Monad
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Sloane.Utils
import Sloane.OEIS

-- | A compact `ByteString` representation of a `Prg`.
newtype PackedPrg = PPrg ByteString deriving (Eq, Show, Generic)

instance ToJSON PackedPrg where
    toJSON (PPrg bs) = String (decodeUtf8 bs)

instance FromJSON PackedPrg where
    parseJSON (String s) = pure $ PPrg (encodeUtf8 s)
    parseJSON _ = mzero

-- | Similary, a packed entry consists of a packed program together with
-- a packed sequence.
data PackedEntry = PackedEntry
    { getPackedPrg :: PackedPrg
    , getPackedSeq :: PackedSeq
    } deriving (Eq, Show, Generic)

instance ToJSON PackedEntry where
    toJSON (PackedEntry p s) =
        object [ "prg" .= toJSON p
               , "seq" .= toJSON ("{" <> s <> "}")
               ]

instance FromJSON PackedEntry where
    parseJSON (Object v) =
        let shave' = PSeq . shave . unPSeq
        in PackedEntry <$> v .: "prg" <*> (shave' <$> v .: "seq")
    parseJSON _ = mzero

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
