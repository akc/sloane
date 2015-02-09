{-# LANGUAGE EmptyDataDecls #-}

-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module Sloane.Types where

import Data.Map (Map)
import Data.ByteString.Char8 (ByteString)

type URL = String

type Key = Char
type ANum = Int
type IntSeq = [Integer]
type Name = ByteString

type PackedANum = ByteString
type PackedSeq  = ByteString

type Reply = Map ANum (Map Char [ByteString])

data Seq
data Names

newtype DB a = DB ByteString
