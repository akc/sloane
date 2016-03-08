{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module Sloane.Entry
    ( Sequence
    , Entry (..)
    ) where

import Data.Ratio
import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Monad
import Control.Applicative

newtype Prg = Prg ByteString deriving (Show, Eq)
type Sequence = [Rational]

instance ToJSON Prg where
    toJSON (Prg bs) = String (decodeUtf8 bs)

instance FromJSON Prg where
    parseJSON (String s) = pure $ Prg (encodeUtf8 s)
    parseJSON _ = mzero

-- | An entry consists of a program together with a list of rational
-- numbers.
data Entry = Entry
    { getPrg :: Prg
    , getSeq :: Sequence
    } deriving (Eq, Show)

instance ToJSON Entry where
    toJSON (Entry prg s) =
        object ([ "hops" .= toJSON prg
                , "nums" .= toJSON (map numerator s)
                ] ++
                [ "dnos" .= toJSON ds
                | let ds = map denominator s
                , any (/=1) ds  -- For terseness only include denominators if
                                -- at least one of them isn't 1
                ])

instance FromJSON Entry where
    parseJSON (Object v) = do
        prg <- v .:  "hops"
        ns  <- v .:  "nums"
        mds <- v .:? "dnos"
        return $ case mds of
             Nothing -> Entry prg (map fromIntegral ns)
             Just ds -> Entry prg (zipWith (%) ns ds)
    parseJSON _ = mzero
