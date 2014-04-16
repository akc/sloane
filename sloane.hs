{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Copyright   : Anders Claesson 2012-2014
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
import Prelude hiding (all)
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Lazy.Char8  as BL
import qualified Data.ByteString.Lazy.Search as Search
import qualified Codec.Compression.GZip      as GZip
import System.IO (stdin, stderr)
import System.Directory
import System.FilePath ((</>))
import System.Console.ANSI
import System.Console.CmdArgs
import System.Console.Terminal.Size (Window(..), size)
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Maybe (maybe, fromJust)
import Control.Monad (when, unless)
import Network.HTTP
import Network.URI (parseURI)

type OEISEntries = [String]
type ANumbers = [String]
type Query = String
type Keys = String

oeisHost  = "http://oeis.org/"
oeisURL   = oeisHost ++ "search?fmt=text"
oeisKeys  = "ISTUVWXNDHFYAOEeptoKC"

cacheDir  = ".sloane"
cacheFile = "stripped.gz"
cacheURL  = oeisHost ++ cacheFile

msgDownloadingCache = unlines
    [ "Downloading " ++ cacheURL
    , "This may take a minute or two ..."
    ]
msgCacheIsUpToDate = unlines
    [ "The sequence cache is now up-to-date"
    ]
msgNoCache = unlines
    [ "No sequence cache found. You need to run \"sloane --update\""
    ]
msgOldCache = unlines
    [ "The sequence cache is more than two weeks old"
    , "You may want to run \"sloane --update\""
    ]

data Sloane = Sloane { all    :: Bool
                     , keys   :: String
                     , limit  :: Int
                     , update :: Bool
                     , url    :: Bool
                     , terms  :: [String]
                     }
              deriving (Data, Typeable)

sloane = cmdArgsMode $ Sloane
  { all = False &= name "a" &= help "Print all fields"
  , keys = "SN" &= typ "KEYS" &= help "Keys of fields to print (default: SN)"
  , limit = 5 &= name "n" &= help "Fetch at most this many entries (default: 5)"
  , update = False &= help "Update the local sequence cache"
  , url = False &= name "u" &= help "Print urls of found entries"
  , terms = def &= args &= typ "SEARCH-TERMS"
  }
  &= versionArg [summary "sloane 1.7"]
  &= summary "Search Sloane's On-Line Encyclopedia of Integer Sequences"

select :: Keys -> OEISEntries -> OEISEntries
select ks = filter (\line -> null line || head line `elem` ks)

aNumbers :: OEISEntries -> ANumbers
aNumbers es = [ words ids !! 1 | ids@(_:_) <- select "I" es ]

urls :: OEISEntries -> String
urls = unlines . map (oeisHost ++ ) . aNumbers

get :: HStream b => String -> IO b
get uri = simpleHTTP (defaultGETRequest_ uri') >>= getResponseBody
  where
    uri' = fromJust $ parseURI uri

updateCache :: FilePath -> IO ()
updateCache home = do
    let dir = home </> cacheDir
    createDirectoryIfMissing False dir
    put msgDownloadingCache
    cache <- get cacheURL
    B.writeFile (dir </> cacheFile) cache
    put msgCacheIsUpToDate

searchOEIS :: Int -> Query -> IO OEISEntries
searchOEIS n s = trim `fmap` get uri
  where
    trim = map (drop 1) . reverse . drop 2 . reverse . drop 5 . lines
    uri = oeisURL ++ "&" ++ urlEncodeVars [("n", show n), ("q", s)]

cropStr :: (Int -> String -> String) -> Int -> String -> String
cropStr f maxLen s = if maxLen < length s then f maxLen s else s

cropSeq :: Int -> String -> String
cropSeq = cropStr $ \maxLen ->
              reverse . dropWhile (/= ',') . reverse . take maxLen

cropLine :: Int -> String -> String
cropLine = cropStr $ \maxLen s -> take (maxLen-2) s ++ ".."

getWidth :: IO Int
getWidth = maybe maxBound width `fmap` size

put = B.putStr . B.pack
putErr = B.hPutStr stderr . B.pack
newline = B.putStrLn B.empty

putEntries :: Int -> OEISEntries -> IO ()
putEntries width = mapM_ $ \line ->
    case words line of
        [] -> newline
        (key:aNum:rest) -> do
            setSGR [ SetColor Foreground Dull Green ]
            put key
            setSGR [ SetColor Foreground Dull Yellow ]
            put $ ' ' : aNum
            setSGR []
            let crop = if key == "S" then cropSeq else cropLine
            put $ ' ' : crop width (unwords rest) ++ "\n"

isInfixOf :: B.ByteString -> BL.ByteString -> Bool
isInfixOf q = not . null . Search.indices q

readCache :: FilePath -> IO BL.ByteString
readCache home = do
    let name = home </> cacheDir </> cacheFile
    updated <- doesFileExist name
    if updated
        then do
            c <- getCurrentTime
            m <- getModificationTime name
            let week = 60*60*24*7
            let expired = c `diffUTCTime` m > 2*week
            when expired $ putErr msgOldCache
            GZip.decompress `fmap` BL.readFile name
        else
            error msgNoCache

seqs :: B.ByteString -> [B.ByteString]
seqs = filter (not . B.null) . map mkSeq . B.lines
  where
    mkSeq = normalize . dropComment
    dropComment = B.takeWhile (/= '#')
    normalize   = B.intercalate (B.pack ",") . B.words . clean . B.map tr
    tr c  = if c `elem` ";," then ' ' else c
    clean = B.filter (\c -> B.elem c (B.pack " 0123456789-"))

filterSeqs :: FilePath -> IO ()
filterSeqs home = do
    cache <- readCache home
    B.getContents >>= mapM_ B.putStrLn . filter (`isInfixOf` cache) . seqs

main = do
    home <- getHomeDirectory
    args <- cmdArgsRun sloane
    let query = unwords $ terms args
    case (update args, null query) of
        (True, _) -> updateCache home
        (_, True) -> filterSeqs home
        _         -> do
            ncols <- getWidth
            hits  <- searchOEIS (limit args) query
            let pick = if all args then id else select (keys args)
            unless (null hits) $ do
                newline
                if url args
                    then put (urls hits)
                    else putEntries (ncols - 10) (pick hits)
                newline
