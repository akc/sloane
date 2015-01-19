-- |
-- Copyright   : Anders Claesson 2014-2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
module Sloane.Config (Config (..), oeisKeys, defaultConfig) where

import System.Console.Terminal.Size (width, size)
import System.FilePath ((</>))
import System.Directory
import Sloane.Types

data Config = Config
    { nameVer     :: String
    , home        :: FilePath
    , sloaneDir   :: FilePath
    , seqDBPath   :: FilePath
    , namesDBPath :: FilePath
    , oeisHost    :: URL
    , oeisURL     :: URL
    , strippedURL :: URL
    , namesURL    :: URL
    , termWidth   :: Int
    }

oeisKeys :: String
oeisKeys = "ISTUVWXNDHFYAOEeptoKC"

defaultConfig :: IO Config
defaultConfig = do
    w <- maybe maxBound width `fmap` size
    h <- getHomeDirectory
    let c = Config { nameVer     = "sloane 2.0.5"
                   , home        = h
                   , sloaneDir   = h </> ".sloane"
                   , seqDBPath   = sloaneDir c </> "stripped"
                   , namesDBPath = sloaneDir c </> "names"
                   , oeisHost    = "https://oeis.org/"
                   , oeisURL     = oeisHost c ++ "search?fmt=text"
                   , strippedURL = oeisHost c ++ "stripped.gz"
                   , namesURL    = oeisHost c ++ "names.gz"
                   , termWidth   = w
                   }
    return c
