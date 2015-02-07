-- |
-- Copyright   : Anders Claesson 2012-2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

import Data.List
import Data.Maybe
import Data.Map ((!))
import qualified Data.Map as M
import Data.Conduit hiding (($$))
import Data.Conduit.Zlib (ungzip)
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Search as S
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Network.HTTP.Conduit
import Options.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import System.Directory
import System.IO
import System.Console.ANSI
import Sloane.Types
import Sloane.Parse
import Sloane.ParseOptions
import Sloane.Config
import Sloane.Transform

grep :: PackedSeq -> SeqDB -> [PackedANum]
grep q db = mapMaybe locateANum (S.indices q db)
  where
    locateANum i = listToMaybe
        [ B.take 7 v
        | j <- [i,i-1..0]
        , B.index db j == 'A'
        , let (_,v) = B.splitAt j db
        ]

filterSeqsIO :: Options -> SeqDB -> IO [PackedSeq]
filterSeqsIO opts db =
    filter memberNotMember . map (parseSeq . dropComment) <$> getNonEmptyLines
  where
    dropComment = B.takeWhile (/='#')
    getNonEmptyLines = filter (not . B.null) . B.lines <$> B.getContents
    memberNotMember q = (if invert opts then id else not ) . null $ grep q db

readSeqDB :: Config -> IO SeqDB
readSeqDB cfg = B.readFile (seqDBPath cfg)

readNamesDB :: Config -> IO NamesDB
readNamesDB cfg = B.readFile (namesDBPath cfg)

putReply :: Config -> Bool -> [Key] -> Reply -> IO ()
putReply cfg noColor ks reply = do
    unless (M.null reply) $ putStrLn ""
    forM_ (M.toList reply) $ \(anum, fields) -> do
        forM_ (ks `intersect` M.keys fields) $ \key -> do
            let entry = fields ! key
            forM_ entry $ \line -> do
                let line' = decodeUtf8 line
                unless noColor $ setSGR [ SetColor Foreground Dull Green ]
                putStr [key]
                unless noColor $ setSGR [ SetColor Foreground Dull Yellow ]
                putStr " " >> B.putStr (packANum anum)
                unless noColor $ setSGR []
                putStr " " >> IO.putStrLn (crop key (termWidth cfg - 10) line')
        putStrLn ""
  where
    cropText f cap s = if cap < T.length s then f cap s else s
    crop key =
      if key `elem` ['S'..'X']
        then cropText $ \cap -> T.reverse . T.dropWhile (/= ',') . T.reverse . T.take cap
        else cropText $ \cap s -> T.take (cap-2) s `T.append` T.pack ".."

putReplyOrUrls :: Options -> Config -> Reply -> IO ()
putReplyOrUrls opts cfg
    | url opts  = putStr . unlines . oeisUrls
    | otherwise = putReply cfg (nocolor opts) keys'
  where
    keys' = if longFormat opts then oeisKeys else keys opts
    oeisUrls = map ((oeisHost cfg ++) . B.unpack . packANum) . M.keys

setQueryStr :: [(String, String)] -> Request -> Request
setQueryStr kvs = setQueryString [(B.pack k, Just (B.pack v)) | (k,v) <- kvs]

oeisLookup :: Int -> String -> Config -> IO Reply
oeisLookup cap term cfg = do
    let kvs = [("n", show cap), ("q", term), ("fmt","text")]
    req <- setQueryStr kvs <$> parseUrl (oeisURL cfg)
    res <- withManager $ httpLbs req
    return $ parseReply $ BL.toStrict $ responseBody res

applyTransform :: Options -> String -> IO ()
applyTransform opts tname =
    case lookupTranform tname of
      Nothing -> error "No transform with that name"
      Just f  -> forM_ (terms opts) $ \input ->
                     case f $$ parseIntSeq (B.pack input) of
                       [] -> return ()
                       cs -> B.putStrLn (packIntSeq cs)

takeUniq :: Eq a => Int -> [a] -> [a]
takeUniq n = take n . map head . group

updateDBs :: Config -> IO ()
updateDBs cfg = do
    createDirectoryIfMissing False (sloaneDir cfg)
    putStr $ "Downloading " ++ strippedURL cfg
    putStr " " >> download (strippedURL cfg) (seqDBPath cfg)
    putStr $ "\nDownloading " ++ namesURL cfg
    putStr " " >> download (namesURL cfg) (namesDBPath cfg)
    putStrLn "\nDone."
  where
    download uri fpath = withManager $ \manager -> do
        req <- parseUrl uri
        res <- http req manager
        responseBody res $$+- progress 0 =$ ungzip =$ CB.sinkFile fpath
    progress cnt = await >>= maybe (return ()) (\bs -> do
        when (cnt `mod` 25 == 0) $ liftIO (putStr "." >> hFlush stdout)
        yield bs
        progress (cnt + 1 :: Integer))

mkReply :: SeqMap -> NamesMap -> [ANum] -> Reply
mkReply s n = M.fromList . map (\k -> (k, M.fromList [('S', [s!k]), ('N', [n!k])]))

sloane :: Options -> Config -> IO ()
sloane opts c
    | version opts = putStrLn (nameVer c)
    | update opts = updateDBs c
    | listTransforms opts = mapM_ (putStrLn . name) transforms
    | anum > 0 = lookupSeq . parseSeqMap <$> readSeqDB c >>= mapM_ B.putStrLn
    | filtr opts = readSeqDB c >>= filterSeqsIO opts >>= mapM_ B.putStrLn
    | not (null tname) = applyTransform opts tname
    | local opts = do
        sm <- parseSeqMap <$> readSeqDB c
        nm <- parseNamesMap <$> readNamesDB c
        let nts = length ts
        forM_ ts $ \term -> do
             let q  = parseSeq (B.pack term)
             let q' = anchorSeq q
             let f  = mkReply sm nm . map parseANum . takeUniq n . grep q'
             when (nts > 1) $ putStr "seq: " >> B.putStrLn q
             readSeqDB c >>= putReplyOrUrls opts c . f
    | otherwise = oeisLookup n (unwords ts) c >>= putReplyOrUrls opts c
  where
    n = if limitless opts then 999999 else limit opts
    ts = terms opts
    anum = anumber opts
    tname = transform opts
    lookupSeq = maybeToList . M.lookup anum

main :: IO ()
main = getOptions >>= \opts -> defaultConfig >>= sloane opts
