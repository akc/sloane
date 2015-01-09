-- |
-- Copyright   : Anders Claesson 2014
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
module Sloane.Config (Config (..), defaultConfig) where

import System.Console.Terminal.Size (width, size)
import System.FilePath ((</>))
import System.Directory

type URL = String

data Config = Config
    { nameVer   :: String
    , home      :: FilePath
    , sloaneDir :: FilePath
    , sloaneDB  :: FilePath
    , oeisHost  :: URL
    , oeisURL   :: URL
    , sURL      :: URL
    , nURL      :: URL
    , termWidth :: Int
    }

defaultConfig :: IO Config
defaultConfig = do
    w <- maybe maxBound width `fmap` size
    h <- getHomeDirectory
    let dsloane = h </> ".sloane"
    return Config
        { nameVer   = "sloane 2.0.3"
        , home      = h
        , sloaneDir = dsloane
        , sloaneDB  = dsloane </> "sloane.db"
        , oeisHost  = oeisorg
        , oeisURL   = oeisorg ++ "search?fmt=text"
        , sURL      = oeisorg ++ "stripped.gz"
        , nURL      = oeisorg ++ "names.gz"
        , termWidth = w
        }
  where
    oeisorg = "https://oeis.org/"
