-- |
-- Copyright   : Anders Claesson
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
module Sloane.Download (requestPage) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.HTTP.Conduit hiding (Proxy)
import Control.Monad.IO.Class (liftIO)
import Sloane.OEIS (URL)

-- | Request a page at a given URL with specified key-value pairs (query
-- string).
requestPage :: URL -> [(ByteString, ByteString)] -> IO ByteString
requestPage url kvs = do
    req <- liftIO $ setQueryString [(k, Just v) | (k,v) <- kvs] <$> parseUrlThrow url
    man <- newManager tlsManagerSettings
    BL.toStrict . responseBody <$> httpLbs req man
