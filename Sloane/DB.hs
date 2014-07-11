-- |
-- Copyright   : Anders Claesson 2014
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module Sloane.DB
    ( DB
    , Reply
    , initDB
    , readDB
    , writeDB
    , putDB
    , null
    , insert
    , lookup
    , grep
    , take
    , aNumbers
    , parseOEISEntries
    ) where

import           Prelude                    hiding (lookup, null, take)
import qualified Prelude                    as P
import           Data.List                  (intersect)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as BL
import           Data.Map                   (Map, (!))
import qualified Data.Map                   as M
import           Data.Serialize
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as IO
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Control.Monad              (forM_, unless)
import qualified Codec.Compression.GZip     as GZip
import           Network.Curl.Download.Lazy (openLazyURI)
import           System.Console.ANSI
import           Sloane.Config
import           System.Directory

type ANumber = Text
type Key     = Char
type Entry   = Text
type Reply   = Map Key Entry

type DB      = Map ANumber Reply
type DBRaw   = Map ByteString (Map Char ByteString)

encodeDB :: DB -> DBRaw
encodeDB = M.mapKeys encodeUtf8 . M.map (M.map encodeUtf8)

decodeDB :: DBRaw -> DB
decodeDB = M.mapKeys decodeUtf8 . M.map (M.map decodeUtf8)

compress :: ByteString -> BL.ByteString
compress = GZip.compress . BL.fromStrict

compressDB :: DB -> BL.ByteString
compressDB = compress . encode . encodeDB

decompress :: BL.ByteString -> ByteString
decompress = BL.toStrict . GZip.decompress

decompressDB :: BL.ByteString -> Either String DB
decompressDB = fmap decodeDB . decode . decompress

initDB :: Config -> IO ()
initDB cfg = do
    createDirectoryIfMissing False (sloaneDir cfg)
    putStrLn $ "Downloading " ++ sURL cfg
    dbS <- openLazyURI (sURL cfg) >>= either error (return . mkDB 'S')
    putStrLn $ "Downloading " ++ nURL cfg
    dbN <- openLazyURI (nURL cfg) >>= either error (return . mkDB 'N')
    putStrLn "Building database"
    writeDB cfg $ unionDB dbS dbN
    putStrLn "Done."
  where
    unionDB = M.unionWith M.union
    mkDB key = mkMap key . decodeUtf8 . decompress
    mkMap key = M.fromList . map (aNumberAndReply key) . drop 4 . T.lines
    mkReply key = M.singleton key . T.dropWhile (==',') . T.drop 8
    aNumberAndReply key line = (T.take 7 line, mkReply key line)

readDB :: Config -> IO DB
readDB cfg = doesFileExist (sloaneDB cfg) >>= \updated ->
    if updated
        then BL.readFile (sloaneDB cfg) >>= either error return . decompressDB
        else error $ "No local database found. " ++
                     "You need to run \"sloane update\" first."

writeDB :: Config -> DB -> IO ()
writeDB cfg = BL.writeFile (sloaneDB cfg) . compressDB

null :: DB -> Bool
null = M.null

insert :: ANumber -> Reply -> DB -> DB
insert = M.insert

lookup :: ANumber -> DB -> Maybe Reply
lookup = M.lookup

grep :: Text -> DB -> DB
grep q = M.filter $ \reply -> q `T.isInfixOf` (reply ! 'S')

take :: Int -> DB -> DB
take n = M.fromList . P.take n . M.toList

aNumbers :: DB -> [ANumber]
aNumbers = M.keys

unions :: [DB] -> DB
unions = M.unionsWith . M.unionWith $ \s t ->
    (s `T.append` T.pack "\n") `T.append` t

singleton :: ANumber -> Key -> Entry -> DB
singleton aNum key entry = M.singleton aNum $ M.singleton key entry

parseOEISEntries :: Text -> DB
parseOEISEntries = unions . map parseLine . trim
  where
    trim = map (T.drop 1) . reverse . drop 2 . reverse . drop 5 . T.lines
    parseLine = parseWords . T.words
    parseWords (key:aNum:rest) = singleton aNum (T.head key) (T.unwords rest)
    parseWords _ = M.empty

putDB :: Config -> [Key] -> DB -> IO ()
putDB cfg keys db = do
    unless (null db) $ putStrLn ""
    forM_ (M.toList db) $ \(aNum, reply) -> do
        forM_ (keys `intersect` M.keys reply) $ \key -> do
            let entry = reply ! key
            forM_ (T.lines entry) $ \line -> do
                setSGR [ SetColor Foreground Dull Green ]
                putStr [key]
                setSGR [ SetColor Foreground Dull Yellow ]
                putStr " " >> IO.putStr aNum
                setSGR []
                putStr " " >> IO.putStrLn (crop key (termWidth cfg - 10) line)
        putStrLn ""

crop :: Key -> Int -> Text -> Text
crop key =
    let cropText f maxLen s = if maxLen < T.length s then f maxLen s else s
    in if key `elem` ['S'..'X']
           then cropText $ \maxLen ->
               T.reverse . T.dropWhile (/= ',') . T.reverse . T.take maxLen
           else cropText $ \maxLen s ->
               T.take (maxLen-2) s `T.append` T.pack ".."
