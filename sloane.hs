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

data Command
    = Lookup SearchOpts
    | Grep   SearchOpts
    | Filter FilterOpts
    | Update
    | Version

data SearchOpts = SearchOpts
    { full   :: Bool     -- Print all fields?
    , keys   :: String   -- Keys of fields to print
    , limit  :: Int      -- Fetch at most this many entries
    , url    :: Bool     -- Print URLs of found entries
    , terms  :: [String] -- Search terms
    }

data FilterOpts = FilterOpts
    { invert :: Bool     -- Return sequences NOT in DB
    }

oeisKeys :: String
oeisKeys = "ISTUVWXNDHFYAOEeptoKC" -- Valid OEIS keys

oeisUrls :: Config -> DB -> [URL]
oeisUrls cfg = map ((oeisHost cfg ++) . T.unpack) . aNumbers

oeisLookup :: SearchOpts -> Config -> IO DB
oeisLookup opts cfg =
    (parseOEISEntries . decodeUtf8 . either error id) <$>
    openURI (oeisURL cfg ++ "&" ++ urlEncodeVars [("n", show n), ("q", q)])
  where
    n = limit opts
    q = unwords $ terms opts

grepDB :: SearchOpts -> DB -> DB
grepDB opts = DB.take n . DB.grep (T.pack q)
  where
    n = limit opts
    q = unwords $ terms opts

filterDB :: FilterOpts -> DB -> IO [Seq]
filterDB opts db = filter match . parseSeqs <$> IO.getContents
  where
    match q = (if invert opts then id else not) (DB.null $ DB.grep q db)
    parseSeqs = filter (not . T.null) . map mkSeq . T.lines
    mkSeq = normalize . dropComment
    dropComment = T.takeWhile (/= '#')
    normalize = T.intercalate (T.pack ",") . T.words . clean . T.map tr
    tr c = if c `elem` ";," then ' ' else c
    clean = T.filter (`elem` " 0123456789-")

searchOptionsParser :: Parser SearchOpts
searchOptionsParser = SearchOpts
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
    <*> switch (long "url" <> help "Print URLs of found entries")
    <*> some (argument str (metavar "TERMS..."))

filterOptionsParser :: Parser FilterOpts
filterOptionsParser = FilterOpts
    <$> switch (long "invert" <> help "Return sequences NOT in the database")

commandParser :: Parser Command
commandParser = h <*> opts
  where
    h = abortOption ShowHelpText $ hidden <> short 'h' <> long "help"
    opts = subparser
        ( command "lookup" (info (Lookup <$> searchOptionsParser)
          ( progDesc "Lookup a sequence, or other search term, in OEIS" ))
       <> command "grep" (info (Grep <$> searchOptionsParser)
          ( progDesc "Grep for a sequence in the local database" ))
       <> command "filter" (info (Filter <$> filterOptionsParser)
          ( progDesc ("Read sequences from stdin and "
                   ++ "return those that are in the local database")))
       <> command "update" (info (pure Update)
          ( progDesc "Update the local database" ))
       <> command "version" (info (pure Version)
          ( progDesc "Show version info" ))
        )

execSearch :: (SearchOpts -> Config -> IO DB) -> SearchOpts -> Config -> IO ()
execSearch f opts cfg = f opts cfg >>=
    if url opts
        then putStr . unlines . oeisUrls cfg
        else putDB cfg (if full opts then oeisKeys else keys opts)

sloane :: Command -> Config -> IO ()
sloane (Lookup opts) = execSearch oeisLookup opts
sloane (Grep   opts) = execSearch (\o cfg -> grepDB o <$> readDB cfg) opts
sloane (Filter opts) = \c -> readDB c >>= filterDB opts >>= mapM_ IO.putStrLn
sloane Update        = initDB
sloane Version       = putStrLn . name

main :: IO ()
main = do
    conf <- defaultConfig
    opts <- customExecParser preferences (info commandParser description)
    sloane opts conf
  where
    preferences = prefs showHelpOnError
    description = fullDesc <> footer
        "Run 'sloane COMMAND --help' for help on a specific command."
