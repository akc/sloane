-- |
-- Copyright   : Anders Claesson 2012-2014
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
import qualified Codec.Compression.GZip       as GZip
import           Control.Monad                (unless, when)
import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy.Char8   as BL
import qualified Data.ByteString.Lazy.Search  as Search
import           Data.Maybe                   (fromJust, maybe)
import           Data.Time                    (diffUTCTime, getCurrentTime)
import           Network.HTTP
import           Network.URI                  (parseURI)
import           Options.Applicative
import           System.Console.ANSI
import           System.Console.Terminal.Size (Window (..), size)
import           System.Directory
import           System.FilePath              ((</>))
import           System.IO                    (stderr, stdin)

type OEISEntries = [String]
type ANumbers = [String]
type Query = String
type Keys = String

data Args = Args
    { all    :: Bool
    , keys   :: String
    , limit  :: Int
    , invert :: Bool
    , update :: Bool
    , url    :: Bool
    , ver    :: Bool
    , terms  :: [String]
    }

version   = "sloane 1.8"

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
    [ "The sequence cache is more than 100 days old"
    , "You may want to run \"sloane --update\""
    ]

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

dropPreamble :: BL.ByteString -> BL.ByteString
dropPreamble = BL.unlines . drop 4 . BL.lines

readCache :: FilePath -> IO BL.ByteString
readCache home = do
    let name = home </> cacheDir </> cacheFile
    updated <- doesFileExist name
    if updated
        then do
            c <- getCurrentTime
            m <- getModificationTime name
            let day = 60*60*24
            let expired = c `diffUTCTime` m > 100*day
            when expired $ putErr msgOldCache
            (dropPreamble . GZip.decompress) `fmap` BL.readFile name
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

filterSeqs :: Bool -> FilePath -> IO ()
filterSeqs invert home = do
    cache <- readCache home
    B.getContents >>= mapM_ B.putStrLn . filter (`f` cache) . seqs
  where
    f s = (if invert then not else id) . isInfixOf s

args :: Parser Args
args = Args
    <$> switch (short 'a' <> long "all" <> help "Print all fields")
    <*> strOption
        ( short 'k'
       <> metavar "KEYS"
       <> value "SN"
       <> help "Keys of fields to print [default: SN]" )
    <*> option
        ( short 'n'
       <> metavar "N"
       <> value 5
       <> help "Fetch at most this many entries [default: 5]" )
    <*> switch
        ( long "invert"
       <> help "When used as a filter, return sequences NOT in OEIS" )
    <*> switch (long "update" <> help "Update the local sequence cache")
    <*> switch (long "url" <> help "Print URLs of found entries")
    <*> switch (hidden <> long "version")
    <*> many (argument str (metavar "TERMS..."))

sloane :: Args -> IO ()
sloane (Args a keys n v update url True ts) = put version >> newline
sloane (Args a keys n v True   url ver  ts) = getHomeDirectory >>= updateCache
sloane (Args a keys n v update url ver  []) = getHomeDirectory >>= filterSeqs v
sloane (Args a keys n v update url ver  ts) = do
    ncols <- getWidth
    hits  <- searchOEIS n (unwords ts)
    let pick = if a then id else select keys
    unless (null hits) $ do
        newline
        if url
            then put (urls hits)
            else putEntries (ncols - 10) (pick hits)
        newline

main = execParser (info (h <*> args) (fullDesc <> header version)) >>= sloane
  where
    h = abortOption ShowHelpText $ hidden <> long "help"
