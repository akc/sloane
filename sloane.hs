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

import Data.List
import Data.Aeson
import Data.Bits (xor)
import Data.Maybe
import Data.Monoid
import Data.Map (Map, (!))
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Control.Applicative
import Control.Monad
import System.Directory
import System.Console.ANSI
import System.IO
import Sloane.OEIS
import Sloane.Entry
import Sloane.Options
import Sloane.Config
import Sloane.Download
import Sloane.Bloom
import Sloane.DB

nameVer  = "sloane 4.0.0"                 :: String
oeisURL  = "https://oeis.org/search"      :: URL
strpdURL = "https://oeis.org/stripped.gz" :: URL
namesURL = "https://oeis.org/names.gz"    :: URL

type Query  = B.ByteString
type Width  = Int
type Limit  = Int
type View   = (Width, Palette, [Key])

data QA = QA Query [Reply]

instance ToJSON QA where
    toJSON (QA q rs) =
        object [ "query" .= String (decodeUtf8 q)
               , "reply" .= toJSON rs
               ]

data Input
    = SearchLocalDB (DB Sequences) (DB Names) Bool Limit View [Either ANum PackedSeq]
    | SearchOEIS Bool Limit View String
    | FilterSeqs (DB Sequences) Bool [PackedEntry]
    | UpdateDBs FilePath FilePath FilePath
    | Empty

data Output
    = OEISReplies View [QA]
    | OEISRepliesJSON [QA]
    | Entries [PackedEntry]
    | NOP

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
        FilterSeqs db (invert opts) . map parsePackedEntryErr <$> readStdin

    | oeis opts =
        return $ SearchOEIS (tojson opts) (limit opts) view (unwords (terms opts))

    | query opts = do
        sdb <- readSeqDB cfg
        ndb <- readNamesDB cfg
        let parseInp t = fromMaybe (error "cannot parse input")
                       $  (Right . getPackedSeq <$> parsePackedEntry t)
                      <|> (Left <$> parseANum t)
                      <|> (Right . packSeq . map fromIntegral <$> parseIntegerSeq t)
        SearchLocalDB sdb ndb (tojson opts) (limit opts) view . map parseInp
            <$> case map B.pack (terms opts) of
                  [] -> readStdin
                  ts -> return ts
    | otherwise = readInput (opts {query = True}) cfg
  where
    view = (termWidth cfg, palette opts, keys opts)

few :: [a] -> Bool
few (_:_:_) = False
few _       = True

allANum :: [QA] -> Bool
allANum= all (\(QA q _) -> B.head q == 'A')

printReply :: View -> Reply -> IO ()
printReply (width, pal, ks) (Reply (ANum anum) (Table table)) =
    forM_ (ks `intersect` M.keys table) $ \key -> do
        let entry = table ! key
        let crop' = crop key (width - 10)
        let mono  = monochrome pal
        forM_ entry $ \line -> do
            unless mono $ setSGR [ SetColor Foreground Dull Green ]
            putStr [key]
            unless mono $ setSGR [ SetColor Foreground Dull Yellow ]
            B.putStr $ indent anum
            unless mono $ setSGR []
            IO.putStrLn $ indent (crop' (decodeUtf8 line))
  where
    indent s = " " <> s
    dropLastField = T.reverse . T.dropWhile (/= ',') . T.reverse
    cropText f cap s = if cap < T.length s then f cap s else s
    crop key = cropText $ \cap s ->
        if key `elem` ['S'..'X']
            then dropLastField (T.take cap s)
            else T.take (cap-2) s <> ".."

printOutput :: Output -> IO ()
printOutput NOP = return ()
printOutput (Entries es) =
    forM_ es $ \(PackedEntry (PPrg p) (PSeq s)) ->
        B.putStrLn $ p <> " => {" <> s <> "}"

printOutput (OEISRepliesJSON rss) = mapM_ (BL.putStrLn . encode) rss

printOutput (OEISReplies view rss) = do
    let hideQuery = few rss || allANum rss
    let noReplies = null $ concat [ rs | QA _ rs <- rss ]
    forM_ (zip [1::Int ..] rss) $ \(i, QA q replies) -> do
        unless hideQuery $ do
            when (i > 1) $ putStrLn ""
            B.putStrLn ("query: " <> q)
        forM_ replies $ \r -> do
            putStrLn ""
            printReply view r
    unless noReplies $ putStrLn ""

-- Construct a list of replies associated with a list of A-numbers.
mkReplies :: Map ANum PackedSeq -> Map ANum Name -> [ANum] -> [Reply]
mkReplies s n anums =
    [ Reply k (Table $ M.fromList [('S', [unPSeq (s!k)]), ('N', [n!k])])
    | k <- anums
    , M.member k s && M.member k n
    ]

sloane :: Input -> IO Output
sloane inp =
    case inp of

      SearchLocalDB sdb ndb jsonflag maxReplies view ts -> do
          let sm = M.fromList $ parseStripped (unDB sdb)
          let nm = M.fromList $ parseNames (unDB ndb)
          let qas = [ QA q (mkReplies sm nm ks)
                    | (q, ks) <-
                        [ case t of
                            Right s@(PSeq r) -> (r, grepN maxReplies s sdb)
                            Left (ANum anum) -> (anum, [ANum anum])
                        | t <- ts
                        ]
                    ]
          return $ if jsonflag then OEISRepliesJSON qas else OEISReplies view qas

      SearchOEIS jsonflag lim view q -> do
          let kvs = [("n", B.pack (show lim)), ("q", B.pack q), ("fmt", "text")]
          replies <- requestPage oeisURL kvs
          let qas = [QA (B.pack q) (parseReplies replies)]
          return $ if jsonflag then OEISRepliesJSON qas else OEISReplies view qas

      FilterSeqs db invFlag es -> do
          let bloom = mkBloomFilter db
          return $ Entries
              [ e | e@(PackedEntry _ s) <- es
              , not (B.null (unPSeq s))
              , invFlag `xor` (s `isFactorOf` bloom && not (null (grep s db)))
              ]

      UpdateDBs sloanedir sdbPath ndbPath -> do
          createDirectoryIfMissing False sloanedir
          let msg1 = "Downloading " ++ strpdURL ++ ": "
          let msg2 = "Downloading " ++ namesURL ++ ": "
          putStr msg1 >> hFlush stdout
          download (length msg1) strpdURL sdbPath >> putStrLn ""
          putStr msg2 >> hFlush stdout
          download (length msg2) namesURL ndbPath >> putStrLn ""
          return NOP

      Empty -> putStrLn nameVer >> return NOP

-- | Main function and entry point for sloane.
main :: IO ()
main = do
    c <- getConfig
    t <- getOptions
    readInput t c >>= sloane >>= printOutput
