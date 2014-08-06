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

data Visibility = Visible | Internal

data Options
    = Cmd Command
    | IntOpts SearchOpts -- Internal opts for fallback to lookup

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

searchOptionsParser :: Visibility -> Parser SearchOpts
searchOptionsParser visibility = hiddenHelp <*> (SearchOpts
    <$> switch
        ( short 'a'
       <> long "all"
       <> help "Print all fields"
       <> f )
    <*> strOption
        ( short 'k'
       <> metavar "KEYS"
       <> value "SN"
       <> help "Keys of fields to print [default: SN]"
       <> f )
    <*> option
        ( short 'n'
       <> metavar "N"
       <> value 5
       <> help "Fetch at most this many entries [default: 5]"
       <> f )
    <*> switch
        ( long "url"
       <> help "Print URLs of found entries"
       <> f )
    <*> some (argument str (metavar "TERMS...")))
  where
    f = case visibility of {Visible -> idm; Internal -> internal}

filterOptionsParser :: Parser FilterOpts
filterOptionsParser = FilterOpts
    <$> switch (long "invert" <> help "Return sequences NOT in the database")

commandParser :: Parser Command
commandParser = subparser
    ( command "lookup" (info (Lookup <$> searchOptionsParser Visible)
      ( progDesc "Lookup a sequence, or other search term, in OEIS" ))
   <> command "grep" (info (Grep <$> searchOptionsParser Visible)
      ( progDesc "Grep for a sequence in the local database" ))
   <> command "filter" (info (Filter <$> filterOptionsParser)
      ( progDesc ("Read sequences from stdin and "
               ++ "return those that are in the local database")))
   <> command "update" (info (pure Update)
      ( progDesc "Update the local database" ))
   <> command "version" (info (pure Version)
      ( progDesc "Show version info" ))
    )

optionsParser :: Parser Options
optionsParser =
    (Cmd <$> commandParser) <|> (IntOpts <$> searchOptionsParser Internal)

runSearch :: (SearchOpts -> Config -> IO DB) -> SearchOpts -> Config -> IO ()
runSearch f opts cfg = f opts cfg >>=
    if url opts
        then putStr . unlines . oeisUrls cfg
        else putDB cfg (if full opts then oeisKeys else keys opts)

runCmd :: Command -> Config -> IO ()
runCmd (Lookup opts) = runSearch oeisLookup opts
runCmd (Grep   opts) = runSearch (\o cfg -> grepDB o <$> readDB cfg) opts
runCmd (Filter opts) = \c -> readDB c >>= filterDB opts >>= mapM_ IO.putStrLn
runCmd Update        = initDB
runCmd Version       = putStrLn . name

hiddenHelp :: Parser (a -> a)
hiddenHelp = abortOption ShowHelpText $ hidden <> short 'h' <> long "help"

main :: IO ()
main = do
    conf <- defaultConfig
    opts <- customExecParser preferences (info parser description)
    case opts of
        (Cmd cmd)   -> runCmd cmd conf
        (IntOpts o) -> runCmd (Lookup o) conf -- Fallback to 'lookup'
  where
    parser = hiddenHelp <*> optionsParser
    preferences = prefs showHelpOnError
    description = fullDesc <> footer
        "Run 'sloane COMMAND --help' for help on a specific command."
