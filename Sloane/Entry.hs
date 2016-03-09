{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module Sloane.Entry
    ( Prg (..)
    , Name (..)
    , Entry (..)
    ) where

import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Monad
import Control.Applicative

newtype Prg = Prg ByteString deriving (Show, Eq)

newtype Name = Name ByteString deriving (Show, Eq)

instance ToJSON Prg where
    toJSON (Prg bs) = String (decodeUtf8 bs)

instance FromJSON Prg where
    parseJSON (String s) = pure $ Prg (encodeUtf8 s)
    parseJSON _ = mzero

instance ToJSON Name where
    toJSON (Name bs) = String (decodeUtf8 bs)

instance FromJSON Name where
    parseJSON (String s) = pure $ Name (encodeUtf8 s)
    parseJSON _ = mzero

-- | An entry consists of a program together with a list of rational
-- numbers.
data Entry = Entry
    { getPrg  :: Prg
    , getSeq  :: [Integer]
    , getDens :: Maybe [Integer]
    , getName :: Maybe Name
    } deriving (Eq, Show)

instance ToJSON Entry where
    toJSON (Entry prg s dens name) =
        object ([ "hops" .= toJSON prg
                , "seq"  .= toJSON s ] ++
                [ "denominators" .= toJSON dens | dens /= Nothing ] ++
                [ "name" .= toJSON name | name /= Nothing ]
               )

instance FromJSON Entry where
    parseJSON (Object v) = do
        prg  <- v .:  "hops"
        ns   <- v .:  "seq"
        dens <- v .:? "denominators"
        name <- v .:? "name"
        return $ Entry prg ns dens name
    parseJSON _ = mzero
