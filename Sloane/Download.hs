-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
module Sloane.Download (download, requestPage) where

import Numeric
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Conduit hiding (($$))
import Data.Conduit.Zlib (ungzip)
import qualified Data.Conduit.Binary as CB
import Network.HTTP.Conduit hiding (Proxy)
import Network.HTTP.Types (hContentLength)
import Control.Monad.IO.Class (liftIO)
import Sloane.OEIS (URL)
import System.Console.ANSI
import System.IO
import Control.Applicative
import Control.Monad.Trans.Resource (runResourceT)

-- | Download a file at a given URL showing a progress indicator at the
-- given column, and save it at a specified path.
download :: Int -> URL -> FilePath -> IO ()
download col url fpath = do
    req <- liftIO $ parseUrl url
    man <- newManager tlsManagerSettings
    runResourceT $ do
        response <- http req man
        let Just cl = lookup hContentLength (responseHeaders response)
        let n = read (B.unpack cl) :: Int
        responseBody response $$+- progress n 0 =$ ungzip =$ CB.sinkFile fpath
  where
    progress total acc = await >>= maybe (return ()) (\chunk -> do
        let acc' = acc + B.length chunk
        let percent = fromIntegral (100*acc') / fromIntegral total :: Double
        liftIO $ setCursorColumn col
        liftIO $ putStr $ showFFloat (Just 2) percent "" ++ "%"
        liftIO $ hFlush stdout
        yield chunk
        progress total acc')

-- | Request a page at a given URL with specified key-value pairs (query
-- string).
requestPage :: URL -> [(ByteString, ByteString)] -> IO ByteString
requestPage url kvs = do
    req <- liftIO $ setQueryString [(k, Just v) | (k,v) <- kvs] <$> parseUrl url
    man <- newManager tlsManagerSettings
    BL.toStrict . responseBody <$> httpLbs req man
