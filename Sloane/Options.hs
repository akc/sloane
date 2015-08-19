-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
-- Command line options for sloane.

module Sloane.Options
    ( Palette
    , monochrome
    , Options (..)
    , getOptions
    ) where

import Options.Applicative
import Sloane.OEIS

-- | A palette is either colorful or monochrome.
data Palette = Colorful | Monochrome deriving (Eq, Enum, Show, Read)

-- | Is the palette monochrome?
monochrome :: Palette -> Bool
monochrome Monochrome = True
monochrome Colorful   = False

-- | Command line options:
data Options = Options
    {
    -- | Search the local DB.
      query          :: Bool
    -- | Search oeis.org.
    , oeis           :: Bool
    -- | Print all fields?
    , longFormat     :: Bool
    -- | Keys of fields to print.
    , keys           :: String
    -- | Fetch at most this many entries.
    , limit          :: Int
    -- | Fetch all entries (that match).
    , limitless      :: Bool
    -- | Should we colorize the output?
    , palette        :: Palette
    -- | Filter out sequences in local DB.
    , tojson         :: Bool
    -- | Return sequences NOT in DB
    , filtr          :: Bool
    -- | Return sequences NOT in DB
    , invert         :: Bool
    -- | Output all the sequences of the local DB
    , update         :: Bool
    -- | Show version info
    , version        :: Bool
    -- Search terms or programs
    , terms          :: [String]
    }

-- | Parse command line options.
optionsParser :: Parser Options
optionsParser =
  abortOption ShowHelpText (long "help") <*> (Options
    <$> switch
        ( short 'q'
       <> long "query"
       <> help "Search the local database" )
    <*> switch
        ( long "oeis"
       <> help "Search oeis.org" )
    <*> switch
        ( long "long"
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
        ( long "all"
       <> help "Fetch all matching entries (equivalent to -n 999999)" )
    <*> (toEnum . fromEnum <$> switch
        ( long "monochrome"
       <> help "Do not colorize the output" ) )
    <*> switch
        ( long "json"
       <> help "Return results is JSON format" )
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
getOptions = updateOpts <$> execParser (info optionsParser fullDesc)
  where
    updateOpts = updateLimit . updateKeys
    updateKeys  opts = if longFormat opts then opts {keys = oeisKeys} else opts
    updateLimit opts = if limitless  opts then opts {limit = 999999}  else opts
