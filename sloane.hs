{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Copyright   : Anders Claesson
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module Main (main) where

import Data.Aeson (decodeStrict, encode)
import Data.Bits (xor)
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Directory
import System.IO
import Sloane.OEIS
import Sloane.Entry
import Sloane.Options
import Sloane.Config
import Sloane.Download
import Sloane.Bloom
import Sloane.DB

versionString :: String
versionString = "5.0.1"

oeisURL :: URL
oeisURL = "https://oeis.org/search"

seqsURL :: URL
seqsURL = "https://oeis.org/stripped.gz"

namesURL :: URL
namesURL = "https://oeis.org/names.gz"

type Limit = Int

data Input
    = SearchLocalDB (DB Seqs) (DB Names) Limit [(Trail, Either ANum PackedSeq)]
    | SearchOEIS Limit String
    | FilterSeqs (DB Seqs) Bool [Entry]
    | UpdateDBs FilePath
    | Empty

data Output
    = OEISReplies [OEISEntry]
    | Entries [Entry]
    | NOP

decodeErr :: B.ByteString -> Entry
decodeErr = fromMaybe (error "error decoding JSON") . decodeStrict

nonEmptyLines :: BL.ByteString -> [B.ByteString]
nonEmptyLines = map BL.toStrict . filter (not . BL.null) . BL.lines

readStdin :: IO [B.ByteString]
readStdin = nonEmptyLines <$> BL.getContents

readInput :: Options -> Config -> IO Input
readInput opts cfg
    | version opts = return Empty

    | update opts =
        return $ UpdateDBs (sloaneDir cfg)

    | filtr opts = do
        db <- readSeqDB cfg
        FilterSeqs db (invert opts) . map decodeErr <$> readStdin

    | oeis opts =
        return $ SearchOEIS (limit opts) (unwords (terms opts))

    | otherwise = do
        sdb <- readSeqDB cfg
        ndb <- readNamesDB cfg
        inp <- case terms opts of
                 [] -> readStdin
                 ts -> return (map B.pack ts)
        let parseInp t =
              case decodeStrict t of
                Just e  -> (getPrg e : getTrail e, Right (packSeq (getSeq e)))
                Nothing -> case parseIntegerSeq t of
                             Just s  -> let s' = packSeq s
                                        in ([Prg ("{" <> unPSeq s' <> "}")], Right s')
                             Nothing -> case parseANum t of
                                          Just a  -> ([], Left a)
                                          Nothing -> error "cannot parse input"
        return $ SearchLocalDB sdb ndb (limit opts) (map parseInp inp)

printOutput :: Output -> IO ()
printOutput NOP = return ()
printOutput (Entries es) = mapM_ (BL.putStrLn . encode) es
printOutput (OEISReplies rs) = mapM_ (BL.putStrLn . encode) rs

-- Construct a list of replies associated with a list of A-numbers.
mkReplies :: Map ANum [Integer] -> Map ANum Name -> [(Trail, ANum)] -> [Entry]
mkReplies s n tas =
    [ Entry (Prg b) (s!a) Nothing (Just (n!a)) trail
    | (trail, a@(ANum b)) <- tas
    , M.member a s && M.member a n
    ]

sloane :: Input -> IO Output
sloane inp =
    case inp of

      SearchLocalDB sdb ndb maxReplies ts -> do
          let sm = M.fromList $ parseStripped (unDB sdb)
          let nm = M.fromList $ parseNames (unDB ndb)
          let anums (trail, Left  a) = [ (trail, a) ]
              anums (trail, Right s) = [ (trail, a) | a <- grepN maxReplies s sdb ]
          return $ Entries (mkReplies sm nm . anums =<< ts)

      SearchOEIS lim q -> do
          let kvs = [("n", B.pack (show lim)), ("q", B.pack q), ("fmt", "text")]
          replies <- requestPage oeisURL kvs
          return $ OEISReplies (parseOEISEntries replies)

      FilterSeqs db invFlag es -> do
          let bloom = mkBloomFilter db
          return $ Entries
              [ e | e <- es
              , let s = packSeq (getSeq e)
              , not (B.null (unPSeq s))
              , invFlag `xor` (s `isFactorOf` bloom && not (null (grep s db)))
              ]

      UpdateDBs sloanedir -> do
          createDirectoryIfMissing False sloanedir
          putStrLn "# You have download two files manually:"
          putStrLn $ "cd " ++ sloanedir
          putStrLn $ "wget " ++ seqsURL
          putStrLn $ "wget " ++ namesURL
          putStrLn "gunzip names.gz stripped.gz"
          putStrLn "cd -"
          hFlush stdout
          return NOP

      Empty -> do
          putStrLn $ "sloane " ++ versionString
          return NOP

-- | Main function and entry point for sloane.
main :: IO ()
main = do
    c <- getConfig
    t <- getOptions
    readInput t c >>= sloane >>= printOutput
