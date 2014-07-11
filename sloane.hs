-- |
-- Copyright   : Anders Claesson 2012-2014
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
import qualified Codec.Compression.GZip       as GZip
import           Control.Monad                (unless, when, liftM2)
import qualified Data.ByteString.Lazy         as BL
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as IO
import           Data.Text.Encoding           (decodeUtf8)
import           Data.Maybe                   (fromJust)
import           Data.Time                    (diffUTCTime, getCurrentTime)
import           Network.HTTP
import           Network.URI                  (parseURI)
import           Options.Applicative
import           System.Console.ANSI
import           System.Console.Terminal.Size (Window (..), size)
import           System.Directory
import           System.FilePath              ((</>))
import           System.IO                    (stderr)

type OEISEntries = [String]
type ANumbers = [String]
type Query = String
type Keys = String

data Args = Args
    Bool     -- a: Print all fields?
    String   -- k: Keys of fields to print
    Int      -- n: Fetch at most this many entries
    Bool     -- invert: Return sequences NOT in OEIS
    Bool     -- update: Update the local sequence cache
    Bool     -- url: Print URLs of found entries
    Bool     -- version
    [String] -- search terms

name, oeisHost, oeisURL, cacheDir, cacheFile, cacheURL :: String

name = "sloane 1.8.2"

oeisHost  = "http://oeis.org/"
oeisURL   = oeisHost ++ "search?fmt=text"

cacheDir  = ".sloane"
cacheFile = "stripped.gz"
cacheURL  = oeisHost ++ cacheFile

msgDownloadingCache, msgCacheIsUpToDate, msgNoCache, msgOldCache :: String

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

put :: String -> IO ()
put = IO.putStr . T.pack

putErr :: String -> IO ()
putErr = IO.hPutStr stderr . T.pack

newline :: IO ()
newline = IO.putStrLn T.empty

putEntries :: Int -> OEISEntries -> IO ()
putEntries ncols = mapM_ $ \line -> do
    case words line of
        []  -> return ()
        [w] -> put w -- Should never be reached
        (key:aNum:rest) -> do
            setSGR [ SetColor Foreground Dull Green ]
            put key
            setSGR [ SetColor Foreground Dull Yellow ]
            put $ ' ' : aNum
            setSGR []
            let crop = if key == "S" then cropSeq else cropLine
            put $ ' ' : crop ncols (unwords rest)
    newline

updateCache :: FilePath -> IO ()
updateCache home = do
    createDirectoryIfMissing False dir
    put msgDownloadingCache
    get cacheURL >>= BL.writeFile (dir </> cacheFile)
    put msgCacheIsUpToDate
  where
    dir = home </> cacheDir

readCache :: FilePath -> IO Text
readCache home = do
    updated <- doesFileExist fname
    if updated
        then do
            age <- liftM2 diffUTCTime getCurrentTime (getModificationTime fname)
            when (age > 100*day) $ putErr msgOldCache
            (dropPreamble . decompress) `fmap` BL.readFile fname
        else
            error msgNoCache
  where
    day = 60*60*24
    fname = home </> cacheDir </> cacheFile
    decompress = decodeUtf8 . BL.toStrict . GZip.decompress
    dropPreamble = T.unlines . drop 4 . T.lines

seqs :: Text -> [Text]
seqs = filter (not . T.null) . map mkSeq . T.lines
  where
    mkSeq = normalize . dropComment
    dropComment = T.takeWhile (/= '#')
    normalize   = T.intercalate (T.pack ",") . T.words . clean . T.map tr
    tr c  = if c `elem` ";," then ' ' else c
    clean = T.filter (`elem` " 0123456789-")

filterSeqs :: Bool -> FilePath -> IO ()
filterSeqs invert home = do
    cache <- readCache home
    let f q = (if invert then not else id) (q `T.isInfixOf` cache)
    IO.getContents >>= mapM_ IO.putStrLn . filter f . seqs

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
sloane (Args _ _    _ _   _    _   True _ ) = put name >> newline
sloane (Args _ _    _ _   True _   _    _ ) = getHomeDirectory >>= updateCache
sloane (Args _ _    _ inv _    _   _    []) = getHomeDirectory >>= filterSeqs inv
sloane (Args a keys n _   _    url _    ts) = do
    ncols <- getWidth
    hits  <- searchOEIS n (unwords ts)
    let pick = if a then id else select keys
    unless (null hits) $ do
        newline
        if url
            then put (urls hits)
            else putEntries (ncols - 10) (pick hits)
        newline

main :: IO ()
main = execParser (info (h <*> args) (fullDesc <> header name)) >>= sloane
  where
    h = abortOption ShowHelpText $ hidden <> long "help"
