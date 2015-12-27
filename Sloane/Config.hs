-- |
-- Copyright   : Anders Claesson 2014-2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
-- Constants determined at runtime such as the home directory.

module Sloane.Config
    ( Config (..)
    , getConfig
    ) where

import System.Console.Terminal.Size (width, size)
import System.FilePath ((</>))
import System.Directory

-- | A data type holding "constants" determined at runtime.
data Config = Config
    {
    -- | The home directory
      home        :: FilePath
    -- | Path to the '.sloane' directory.
    , sloaneDir   :: FilePath
    -- | Path to 'stripped' file.
    , seqDBPath   :: FilePath
    -- | Path to 'names' file.
    , namesDBPath :: FilePath
    -- | The width of the terminal.
    , termWidth   :: Int
    }

-- | Get configuration.
getConfig :: IO Config
getConfig = do
    w <- maybe maxBound width `fmap` size
    h <- getHomeDirectory
    let c = Config { home        = h
                   , sloaneDir   = h </> ".oeis-data"
                   , seqDBPath   = sloaneDir c </> "stripped"
                   , namesDBPath = sloaneDir c </> "names"
                   , termWidth   = w
                   }
    return c
