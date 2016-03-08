{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Copyright   : Anders Claesson 2012-2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module Main (main) where

import Data.Aeson
import Data.Bits (xor)
import Data.Maybe
import Data.Ratio
import Data.Map (Map, (!))
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text.Encoding (decodeUtf8)
import Control.Applicative
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
versionString = "4.2.0"

oeisURL :: URL
oeisURL = "https://oeis.org/search"

seqsURL :: URL
seqsURL = "https://oeis.org/stripped.gz"

namesURL :: URL
namesURL = "https://oeis.org/names.gz"

type Query = B.ByteString
type Limit = Int

data QA = QA Query [OEISEntry]

instance ToJSON QA where
    toJSON (QA q rs) =
        object [ "query" .= String (decodeUtf8 q)
               , "reply" .= toJSON rs
               ]

data Input
    = SearchLocalDB (DB Sequences) (DB Names) Limit [Either ANum PackedSeq]
    | SearchOEIS Limit String
    | FilterSeqs (DB Sequences) Bool [Entry]
    | UpdateDBs FilePath FilePath FilePath
    | Empty

data Output
    = OEISReplies [QA]
    | Entries [Entry]
    | Version String
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
        return $ UpdateDBs (sloaneDir cfg) (seqDBPath cfg) (namesDBPath cfg)

    | filtr opts = do
        db <- readSeqDB cfg
        FilterSeqs db (invert opts) . map decodeErr <$> readStdin

    | oeis opts =
        return $ SearchOEIS (limit opts) (unwords (terms opts))

    | otherwise = do
        sdb <- readSeqDB cfg
        ndb <- readNamesDB cfg
        let parseInp t = fromMaybe (error "cannot parse input")
                       $  (Right . packSeq . map numerator . getSeq <$> decodeStrict t)
                      <|> (Left <$> parseANum t)
                      <|> (Right . packSeq <$> parseIntegerSeq t)
        SearchLocalDB sdb ndb (limit opts) . map parseInp
            <$> case map B.pack (terms opts) of
                  [] -> readStdin
                  ts -> return ts

printOutput :: Output -> IO ()
printOutput NOP = return ()
printOutput (Version s) = putStrLn $ "sloane " ++ s
printOutput (Entries es) = mapM_ (BL.putStrLn . encode) es
printOutput (OEISReplies rs) = mapM_ (BL.putStrLn . encode) rs

-- Construct a list of replies associated with a list of A-numbers.
mkReplies :: Map ANum [Integer] -> Map ANum Name -> [ANum] -> [Entry]
mkReplies s n anums =
    [ Entry (Prg b) (map fromIntegral (s!k)) (Just (n!k))
    | k@(ANum b) <- anums
    , M.member k s && M.member k n
    ]

sloane :: Input -> IO Output
sloane inp =
    case inp of

      SearchLocalDB sdb ndb maxReplies ts -> do
          let sm = M.fromList $ parseStripped (unDB sdb)
          let nm = M.fromList $ parseNames (unDB ndb)
          let es = [ mkReplies sm nm ks
                   | ks <- [ case t of
                               Left anum -> [anum]
                               Right s   -> grepN maxReplies s sdb
                           | t <- ts
                           ]
                   ]
          return $ Entries (concat es)

      SearchOEIS lim q -> do
          let kvs = [("n", B.pack (show lim)), ("q", B.pack q), ("fmt", "text")]
          replies <- requestPage oeisURL kvs
          let qas = [QA (B.pack q) (parseOEISEntries replies)]
          return $ OEISReplies qas

      FilterSeqs db invFlag es -> do
          let bloom = mkBloomFilter db
          return $ Entries
              [ e | e@(Entry _ s _) <- es
              , not (null s)
              , let t = packSeq (map numerator s)
              , invFlag `xor` (t `isFactorOf` bloom && not (null (grep t db)))
              ]

      UpdateDBs sloanedir sdbPath ndbPath -> do
          createDirectoryIfMissing False sloanedir
          let msg1 = "Downloading " ++ seqsURL ++ ": "
          let msg2 = "Downloading " ++ namesURL ++ ": "
          putStr msg1 >> hFlush stdout
          download (length msg1) seqsURL sdbPath >> putStrLn ""
          putStr msg2 >> hFlush stdout
          download (length msg2) namesURL ndbPath >> putStrLn ""
          return NOP

      Empty -> return $ Version versionString

-- | Main function and entry point for sloane.
main :: IO ()
main = do
    c <- getConfig
    t <- getOptions
    readInput t c >>= sloane >>= printOutput
