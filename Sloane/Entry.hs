{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright   : Anders Claesson
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module Sloane.Entry
    ( Prg (..)
    , Name (..)
    , Trail
    , Entry (..)
    ) where

import Data.Aeson
import Data.Maybe
import Data.ByteString.Char8 (ByteString)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Monad

newtype Prg = Prg ByteString deriving (Show, Eq)

type Trail = [Prg]

newtype Name = Name ByteString deriving (Show, Eq)

instance ToJSON Prg where
    toJSON (Prg bs) = String (decodeUtf8 bs)

instance FromJSON Prg where
    parseJSON (String s) = return $ Prg (encodeUtf8 s)
    parseJSON _ = mzero

instance ToJSON Name where
    toJSON (Name bs) = String (decodeUtf8 bs)

instance FromJSON Name where
    parseJSON (String s) = return $ Name (encodeUtf8 s)
    parseJSON _ = mzero

-- | An entry consists of a program together with a list of rational
-- numbers.
data Entry = Entry
    { getPrg   :: Prg
    , getSeq   :: [Integer]
    , getDens  :: Maybe [Integer]
    , getName  :: Maybe Name
    , getTrail :: [Prg]
    } deriving (Eq, Show)

instance ToJSON Entry where
    toJSON (Entry prg s dens name trail) =
        object ([ "hops"         .= toJSON prg
                , "seq"          .= toJSON s ] ++
                [ "denominators" .= toJSON dens  | isJust dens ] ++
                [ "name"         .= toJSON name  | isJust name ] ++
                [ "trail"        .= toJSON trail | not (null trail) ]
               )

instance FromJSON Entry where
    parseJSON (Object v) = do
        prg   <- v .:  "hops"
        ns    <- v .:  "seq"
        dens  <- v .:? "denominators"
        name  <- v .:? "name"
        trail <- v .:? "trail"
        return $ Entry prg ns dens name (fromMaybe [] trail)
    parseJSON _ = mzero
