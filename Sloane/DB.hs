-- |
-- Copyright   : Anders Claesson 2014-2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module Sloane.DB
    ( DB
    , ANumber
    , Seq
    , Reply
    , update
    , read
    , write
    , put
    , null
    , insert
    , lookup
    , lookupSeq
    , grep
    , take
    , aNumbers
    , parseOEISEntries
    ) where

import           Prelude                    hiding (lookup, null, take, read)
import qualified Prelude                    as P
import           Data.List                  (intersect)
import qualified Data.ByteString.Search     as Search
import qualified Data.ByteString.Char8      as Ch8
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import           Data.Map.Strict            (Map, (!))
import qualified Data.Map.Strict            as M
import           Data.Serialize             hiding (put)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as IO
import           Data.Text.Encoding         (decodeUtf8)
import           Control.Monad              (forM_, unless)
import           Control.Applicative        ((<$>))
import qualified Codec.Compression.GZip     as GZ
import           Network.Curl.Download      (openURI)
import           System.Console.ANSI
import           Sloane.Config
import           System.Directory

type ANumber = ByteString
type Seq     = ByteString
type Key     = Char
type Entry   = ByteString
type Reply   = Map Key Entry

type DB      = Map ANumber Reply

gzCompress :: ByteString -> ByteString
gzCompress = BL.toStrict . GZ.compress . BL.fromStrict

compressDB :: DB -> ByteString
compressDB = gzCompress . encode

gzDecompress :: ByteString -> ByteString
gzDecompress = BL.toStrict . GZ.decompress . BL.fromStrict

decompressDB :: ByteString -> Either String DB
decompressDB = decode . gzDecompress

update :: Config -> IO ()
update cfg = do
    createDirectoryIfMissing False (sloaneDir cfg)
    putStrLn $ "Downloading " ++ sURL cfg
    dbS <- either error (mkDB 'S') <$> openURI (sURL cfg)
    putStrLn $ "Downloading " ++ nURL cfg
    dbN <- either error (mkDB 'N') <$> openURI (nURL cfg)
    putStrLn "Building database"
    write cfg $ unionDB dbS dbN
    putStrLn "Done."
  where
    unionDB = M.unionWith M.union
    mkDB key = mkMap key . gzDecompress
    mkMap key = M.fromList . map (aNumberAndReply key) . drop 4 . Ch8.lines
    mkReply key = M.singleton key . Ch8.dropWhile (==',') . Ch8.drop 8
    aNumberAndReply key line = (Ch8.take 7 line, mkReply key line)

read :: Config -> IO DB
read cfg = doesFileExist (sloaneDB cfg) >>= \updated ->
    if updated
        then either error id . decompressDB <$> B.readFile (sloaneDB cfg)
        else error $ "No local database found. " ++
                     "You need to run \"sloane --update\" first."

write :: Config -> DB -> IO ()
write cfg = B.writeFile (sloaneDB cfg) . compressDB

null :: DB -> Bool
null = M.null

insert :: ANumber -> Reply -> DB -> DB
insert = M.insert

lookup :: ANumber -> DB -> Maybe Reply
lookup = M.lookup

lookupSeq :: ANumber -> DB -> Maybe Seq
lookupSeq anum db = lookup anum db >>= M.lookup 'S'

isInfix :: ByteString -> ByteString -> Bool
isInfix q = not . P.null . Search.indices q

grep :: ByteString -> DB -> DB
grep q = M.filter $ \reply ->
             case M.lookup 'S' reply of
                 Nothing -> False
                 Just r  -> q `isInfix` r

take :: Int -> DB -> DB
take n = M.fromList . P.take n . M.toList

aNumbers :: DB -> [ANumber]
aNumbers = M.keys

unions :: [DB] -> DB
unions = M.unionsWith . M.unionWith $ \s t ->
    (s `B.append` Ch8.pack "\n") `B.append` t

singleton :: ANumber -> Key -> Entry -> DB
singleton aNum key = M.singleton aNum . M.singleton key

parseOEISEntries :: ByteString -> DB
parseOEISEntries = unions . map parseLine . trim
  where
    trim = map (Ch8.drop 1) . reverse . drop 2 . reverse . drop 5 . Ch8.lines
    parseLine = parseWords . Ch8.words
    parseWords (key:aNum:rest) = singleton aNum (Ch8.head key) (Ch8.unwords rest)
    parseWords _ = M.empty

put :: Config -> [Key] -> DB -> IO ()
put cfg keys db = do
    unless (null db) $ putStrLn ""
    forM_ (M.toList db) $ \(aNum, reply) -> do
        forM_ (keys `intersect` M.keys reply) $ \key -> do
            let entry = reply ! key
            forM_ (Ch8.lines entry) $ \line -> do
                let line' = decodeUtf8 line
                setSGR [ SetColor Foreground Dull Green ]
                putStr [key]
                setSGR [ SetColor Foreground Dull Yellow ]
                putStr " " >> Ch8.putStr aNum
                setSGR []
                putStr " " >> IO.putStrLn (crop key (termWidth cfg - 10) line')
        putStrLn ""

crop :: Key -> Int -> Text -> Text
crop key =
    let cropText f maxLen s = if maxLen < T.length s then f maxLen s else s
    in if key `elem` ['S'..'X']
           then cropText $ \maxLen ->
               T.reverse . T.dropWhile (/= ',') . T.reverse . T.take maxLen
           else cropText $ \maxLen s ->
               T.take (maxLen-2) s `T.append` T.pack ".."
