-- |
-- Copyright   : Anders Claesson 2015, 2016
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
-- Command line options for sloane.

module Sloane.Options
    ( Options (..)
    , getOptions
    ) where

import Options.Applicative

-- | Command line options:
data Options = Options
    {
    -- | Search oeis.org.
      oeis           :: Bool
    -- | Fetch at most this many entries.
    , limit          :: Int
    -- | Fetch all entries (that match).
    , limitless      :: Bool
    -- | Filter out sequences in local DB.
    , filtr          :: Bool
    -- | Return sequences NOT in DB
    , invert         :: Bool
    -- | Output all the sequences of the local DB
    , update         :: Bool
    -- | Show version info
    , version        :: Bool
    -- | Search terms or hops entries
    , terms          :: [String]
    }

-- | Parse command line options.
optionsParser :: Parser Options
optionsParser =
  abortOption ShowHelpText (long "help") <*> (Options
    <$> switch
        ( long "oeis"
       <> help "Search oeis.org" )
    <*> option auto
        ( short 'n'
       <> metavar "N"
       <> value 5
       <> help "Fetch at most this many entries [default: 5]" )
    <*> switch
        ( long "all"
       <> help "Fetch all matching entries (equivalent to -n 999999)" )
    <*> switch
        ( long "filter"
       <> help ("Read sequences from stdin and return"
            ++ " those that are in the local database") )
    <*> switch
        ( long "invert"
       <> help "When filtering return sequences NOT in the database." )
    <*> switch
        ( long "update"
       <> help "Update the local database" )
    <*> switch
        ( long "version"
       <> help "Show version info" )
    <*> many (argument str (metavar "TERMS...")))

-- | Run the command line options parser (above).
getOptions :: IO Options
getOptions = updateLimit <$> execParser (info optionsParser fullDesc)
  where
    updateLimit opts = if limitless opts then opts {limit = 999999} else opts
