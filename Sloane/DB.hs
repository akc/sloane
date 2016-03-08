{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module Sloane.DB
    ( DB (..), Seqs, Names
    , readSeqDB
    , readNamesDB
    , grepN
    , grep
    ) where

import Control.Applicative
import Data.Maybe
import Data.List
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Search as S
import System.Directory
import Sloane.OEIS
import Sloane.Config

-- | An empty data declaration used with the phantom `DB` data type.
data Seqs

-- | An empty data declaration used with the phantom `DB` data type.
data Names

-- | A data base (DB) is just a wrapped `ByteString`.
newtype DB a = DB {unDB :: ByteString} deriving Show

-- | Read the DB at the given location.
readDB :: FilePath -> IO (DB a)
readDB fpath = doesFileExist fpath >>= \b ->
    if b then DB <$> B.readFile fpath
         else error "No local database; run 'sloane --update' first."

-- | Read the sequence DB (derived from \"stripped.gz\").
readSeqDB :: Config -> IO (DB Seqs)
readSeqDB = readDB . seqDBPath

-- | Read the names DB (derived from \"names.gz\").
readNamesDB :: Config -> IO (DB Names)
readNamesDB = readDB . namesDBPath

-- | Return all A-numbers whose associated sequence contains a given
-- sequence as a factor.
grep :: PackedSeq -> DB Seqs -> [ANum]
grep (PSeq p) (DB bs) = mapMaybe locateANum (S.indices q bs)
  where
    q = B.snoc (B.cons ',' p) ','
    locateANum i = listToMaybe
        [ ANum (B.take 7 v)
        | j <- [i,i-1..0]
        , B.index bs j == 'A'
        , let (_,v) = B.splitAt j bs
        ]

-- | Similar to `grep` but return at most 'n' unique hits.
grepN :: Int -> PackedSeq -> DB Seqs -> [ANum]
grepN n q db = Prelude.take n $ map head $ group (grep q db)
