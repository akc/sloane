-- |
-- Copyright   : Anders Claesson 2012-2014
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as IO
import           Data.Text.Encoding           (decodeUtf8)
import           Network.HTTP                 (urlEncodeVars)
import           Network.Curl.Download        (openURI)
import           Options.Applicative
import           Sloane.Config
import           Sloane.DB                    hiding (null)
import qualified Sloane.DB                    as DB

type URL = String
type Seq = Text

data Options = Options
    { local   :: Bool     -- Lookup in local DB
    , filtr   :: Bool     -- Filter out sequences in local DB
    , invert  :: Bool     -- Return sequences NOT in DB
    , update  :: Bool     -- Updated local DB
    , version :: Bool     -- Show version info
    , full    :: Bool     -- Print all fields?
    , keys    :: String   -- Keys of fields to print
    , limit   :: Int      -- Fetch at most this many entries
    , url     :: Bool     -- Print URLs of found entries
    , terms   :: [String] -- Search terms
    }

oeisKeys :: String
oeisKeys = "ISTUVWXNDHFYAOEeptoKC" -- Valid OEIS keys

oeisUrls :: Config -> DB -> [URL]
oeisUrls cfg = map ((oeisHost cfg ++) . T.unpack) . aNumbers

oeisLookup :: Options -> Config -> IO DB
oeisLookup opts cfg =
    (parseOEISEntries . decodeUtf8 . either error id) <$>
    openURI (oeisURL cfg ++ "&" ++ urlEncodeVars [("n", show n), ("q", q)])
  where
    n = limit opts
    q = unwords $ terms opts

grepDB :: Options -> DB -> DB
grepDB opts = DB.take n . DB.grep (T.pack q)
  where
    n = limit opts
    q = unwords $ terms opts

dropComment :: Text -> Text
dropComment = T.takeWhile (/= '#')

mkSeq :: Text -> Seq
mkSeq = T.intercalate (T.pack ",") . T.words . clean . dropComment
  where
    clean = T.filter (`elem` " 0123456789-") . T.map tr
    tr c  = if c `elem` ";," then ' ' else c

filterDB :: Options -> DB -> IO [Seq]
filterDB opts db = filter match . parseSeqs <$> IO.getContents
  where
    match q = (if invert opts then id else not) (DB.null $ DB.grep q db)
    parseSeqs = filter (not . T.null) . map mkSeq . T.lines

hiddenHelp :: Parser (a -> a)
hiddenHelp = abortOption ShowHelpText $ hidden <> short 'h' <> long "help"

optionsParser :: Parser Options
optionsParser = hiddenHelp <*> (Options
    <$> switch
        ( long "local"
       <> help "Use the local database rather than oeis.org" )
    <*> switch
        ( long "filter"
       <> help ("Read sequences from stdin and return"
            ++ " those that are in the local database") )
    <*> switch
        ( long "invert"
       <> help ("Return sequences NOT in the database;"
            ++ " only relevant when used with --filter") )
    <*> switch
        ( long "update"
       <> help "Update the local database" )
    <*> switch
        ( long "version"
       <> help "Show version info" )
    <*> switch
        ( short 'a'
       <> long "all"
       <> help "Print all fields" )
    <*> strOption
        ( short 'k'
       <> metavar "KEYS"
       <> value "SN"
       <> help "Keys of fields to print [default: SN]" )
    <*> option auto
        ( short 'n'
       <> metavar "N"
       <> value 5
       <> help "Fetch at most this many entries [default: 5]" )
    <*> switch
        ( long "url"
       <> help "Print URLs of found entries" )
    <*> some (argument str (metavar "TERMS...")))

search :: (Options -> Config -> IO DB) -> Options -> Config -> IO ()
search f opts cfg = f opts cfg >>= put
  where
    put | url opts  = putStr . unlines . oeisUrls cfg
        | otherwise = putDB cfg $ if full opts then oeisKeys else keys opts

sloane :: Options -> Config -> IO ()
sloane opts
  | version opts = putStrLn . name
  | update  opts = initDB
  | filtr   opts = \c -> readDB c >>= filterDB opts >>= mapM_ IO.putStrLn
  | local   opts = search (\o cfg -> grepDB o <$> readDB cfg) opts
  | otherwise    = search oeisLookup opts

main :: IO ()
main = do
  let pprefs = prefs showHelpOnError
  let pinfo = info optionsParser fullDesc
  opts <- customExecParser pprefs pinfo
  conf <- defaultConfig
  sloane opts conf
