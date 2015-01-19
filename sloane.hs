-- |
-- Copyright   : Anders Claesson 2012-2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

import           Data.List                    (intercalate)
import           Data.Maybe                   (maybeToList)
import           Data.Monoid
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as Ch8
import           System.IO                    (isEOF)
import           Network.HTTP                 (urlEncodeVars)
import           Network.Curl.Download        (openURI)
import           Options.Applicative
import           Sloane.Config
import           Sloane.DB                    (DB, ANumber, Seq)
import qualified Sloane.DB                    as DB
import           Sloane.Transform

type URL = String

data Options = Options
    { full    :: Bool     -- Print all fields?
    , keys    :: String   -- Keys of fields to print
    , limit   :: Int      -- Fetch at most this many entries
    , url     :: Bool     -- Print URLs of found entries
    , local   :: Bool     -- Lookup in local DB
    , anumber :: Int      -- Get the sequence with this number from the local DB
    , filtr   :: Bool     -- Filter out sequences in local DB
    , invert  :: Bool     -- Return sequences NOT in DB
    , transform :: String -- Apply the named transform
    , listTransforms :: Bool -- List the names of all transforms
    , update  :: Bool     -- Updated local DB
    , version :: Bool     -- Show version info
    , terms   :: [String] -- Search terms
    }

oeisKeys :: String
oeisKeys = "ISTUVWXNDHFYAOEeptoKC" -- Valid OEIS keys

oeisUrls :: Config -> DB -> [URL]
oeisUrls cfg = map ((oeisHost cfg ++) . Ch8.unpack) . DB.aNumbers

oeisLookup :: Options -> Config -> IO DB
oeisLookup opts cfg =
    (either error DB.parseOEISEntries) <$>
    openURI (oeisURL cfg ++ "&" ++ urlEncodeVars [("n", show n), ("q", q)])
  where
    n = limit opts
    q = unwords $ terms opts

grepDB :: Options -> DB -> DB
grepDB opts = DB.take n . DB.grep (Ch8.pack q)
  where
    n = limit opts
    q = intercalate "," (terms opts)

applyTransform :: Options -> String -> IO ()
applyTransform opts tname =
    case lookupTranform tname of
      Nothing -> error "No transform with that name"
      Just f  -> case f $$ input of
                   [] -> return ()
                   cs -> putStrLn (showSeq cs)
  where
    tr c  = if c `elem` ";," then ' ' else c
    input = map read (words (map tr (unwords (terms opts))))

dropComment :: ByteString -> ByteString
dropComment = Ch8.takeWhile (/= '#')

showSeq :: [Integer] -> String
showSeq = intercalate "," . map show

mkSeq :: ByteString -> Seq
mkSeq = Ch8.intercalate (Ch8.pack ",") . Ch8.words . clean . dropComment
  where
    clean = Ch8.filter (`elem` " 0123456789-") . Ch8.map tr
    tr c  = if c `elem` ";," then ' ' else c

mkANumber :: Int -> ANumber
mkANumber n = let s = show n in Ch8.pack ('A' : replicate (6-length s) '0' ++ s)

filterDB :: Options -> DB -> IO [Seq]
filterDB opts db = filter match . parseSeqs <$> Ch8.getContents
  where
    match q = (if invert opts then id else not) (DB.null $ DB.grep q db)
    parseSeqs = filter (not . Ch8.null) . map mkSeq . Ch8.lines

optionsParser :: Parser Options
optionsParser =
  abortOption ShowHelpText (long "help") <*> (Options
    <$> switch
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
    <*> switch
        ( long "local"
       <> help "Use the local database rather than oeis.org" )
    <*> option auto
        ( short 'A'
       <> metavar "NUMBER"
       <> value 0
       <> help "Fetch the sequence with this number from the local database" )
    <*> switch
        ( long "filter"
       <> help ("Read sequences from stdin and return"
            ++ " those that are in the local database") )
    <*> switch
        ( long "invert"
       <> help ("Return sequences NOT in the database;"
            ++ " only relevant when used with --filter") )
    <*> strOption
        ( long "transform"
       <> metavar "NAME"
       <> value ""
       <> help "Apply the named transform to input sequence")
    <*> switch
        ( long "list-transforms"
       <> help "List the names of all transforms" )
    <*> switch
        ( long "update"
       <> help "Update the local database" )
    <*> switch
        ( long "version"
       <> help "Show version info" )
    <*> many (argument str (metavar "TERMS...")))

search :: (Options -> Config -> IO DB) -> Options -> Config -> IO ()
search f opts cfg = f opts cfg >>= put
  where
    put | url opts  = putStr . unlines . oeisUrls cfg
        | otherwise = DB.put cfg $ if full opts then oeisKeys else keys opts

getTerms :: Options -> IO Options
getTerms opts
    | dont      = return opts
    | otherwise = (\xs -> opts {terms = xs}) <$>
        case terms opts of
          [] -> isEOF >>= \b ->
                  if b then error "<stdin>: end of file"
                       else return <$> getLine
          ts -> return ts
  where
    dont = or [ filtr opts, anumber opts > 0
              , listTransforms opts, update opts
              , version opts
              ]

main :: IO ()
main = do
    opts <- getTerms =<< execParser (info optionsParser fullDesc)
    let tname = transform opts
    let anum = anumber opts
    let lookupSeq = maybeToList . DB.lookupSeq (mkANumber anum)
    let sloane
         | version opts = putStrLn . nameVer
         | update opts = DB.update
         | listTransforms opts = const $ mapM_ (putStrLn . name) transforms
         | anum > 0 = \c -> lookupSeq <$> DB.read c >>= mapM_ Ch8.putStrLn
         | filtr opts = \c -> DB.read c >>= filterDB opts >>= mapM_ Ch8.putStrLn
         | not (null tname) = const $ applyTransform opts tname
         | local opts = search (\o cfg -> grepDB o <$> DB.read cfg) opts
         | otherwise = search oeisLookup opts
    defaultConfig >>= sloane
