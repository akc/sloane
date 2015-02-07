-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module Sloane.ParseOptions (Options (..), getOptions) where

import Options.Applicative
import System.IO

data Options = Options
    { longFormat     :: Bool     -- Print all fields?
    , keys           :: String   -- Keys of fields to print
    , limit          :: Int      -- Fetch at most this many entries
    , limitless      :: Bool     -- Fetch all entries (that match)
    , local          :: Bool     -- Lookup in local DB
    , url            :: Bool     -- Print URLs of found entries
    , nocolor        :: Bool     -- Do not colorize the output
    , anumber        :: Int      -- Get this sequence from the local DB
    , filtr          :: Bool     -- Filter out sequences in local DB
    , invert         :: Bool     -- Return sequences NOT in DB
    , transform      :: String   -- Apply the named transform
    , listTransforms :: Bool     -- List the names of all transforms
    , update         :: Bool     -- Updated local DB
    , version        :: Bool     -- Show version info
    , terms          :: [String] -- Search terms
    }

optionsParser :: Parser Options
optionsParser =
  abortOption ShowHelpText (long "help") <*> (Options
    <$> switch
        ( short 'l'
       <> long "long"
       <> help "Long format; print all fields" )
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
        ( short 'a'
       <> long "all"
       <> help "Fetch all matching entries (equivalent to -n 999999)" )
    <*> switch
        ( short 'q'
       <> long "quick"
       <> help ("Do a 'quick' offline search for a sequence using the local"
            ++ " database rather than oeis.org" ) )
    <*> switch
        ( long "url"
       <> help "Print URLs of found entries" )
    <*> switch
        ( long "nocolor"
       <> help "Do not colorize the output" )
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
        ( short 't'
       <> long "transform"
       <> metavar "NAME"
       <> value ""
       <> help "Apply the named transform to input sequences" )
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

getTerms :: Options -> IO Options
getTerms opts
    | doNothing = return opts
    | otherwise = (\xs -> opts {terms = xs}) <$>
        case terms opts of
          [] -> isEOF >>= \b ->
                  if b then error "<stdin>: end of file"
                       else filter (not . null) . lines <$> getContents
          ts -> return ts
  where
    doNothing = version opts || anumber opts > 0 || filtr opts
              || update opts || listTransforms opts

setKeys :: Options -> Options
setKeys opts =
    if longFormat opts then opts {keys = "ISTUVWXNDHFYAOEeptoKC"} else opts

getOptions :: IO Options
getOptions = setKeys <$> execParser (info optionsParser fullDesc) >>= getTerms
