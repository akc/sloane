{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Copyright   : Anders Claesson 2012, 2013, 2014
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
import Prelude hiding (all, putStrLn, putStr)
import Data.ByteString.Char8 (putStrLn, putStr, pack, empty)
import System.Console.ANSI
import System.Console.CmdArgs
import System.Console.Terminal.Size (Window(..), size)
import Data.Maybe (maybe, fromJust)
import Control.Monad (unless, guard)
import Network.HTTP
import Network.URI (parseURI)

type OEISEntries = [String]
type ANumbers = [String]
type Query = String
type Keys = String

oeisHost = "http://oeis.org"
oeisURL  = oeisHost ++ "/search?fmt=text"
oeisKeys = "ISTUVWXNDHFYAOEeptoKC"

data Sloane = Sloane { keys  :: String
                     , all   :: Bool
                     , url   :: Bool
                     , limit :: Int
                     , terms :: [String]
                     }
              deriving (Data, Typeable)

sloane = cmdArgsMode $ Sloane
  { keys = "SN"  &= typ "KEYS" &= help "Keys of fields to print (default: SN)"
  , all  = False &= name "a"   &= help "Print all fields"
  , url  = False &= name "u"   &= help "Print urls of found entries"
  , limit = 5 &= name "n" &= help "Retrieve at most this many entries (default: 5)"
  , terms = def &= args &= typ "SEARCH-TERMS"
  }
  &= versionArg [summary "sloane 1.6.1"]
  &= summary "Search Sloane's On-Line Encyclopedia of Integer Sequences"

select :: Keys -> OEISEntries -> OEISEntries
select ks = filter (\line -> null line || head line `elem` ks)

aNumbers :: OEISEntries -> ANumbers
aNumbers es = [ words ids !! 1 | ids@(_:_) <- select "I" es ]

urls :: OEISEntries -> String
urls = unlines . map ((oeisHost ++ "/") ++ ) . aNumbers

get :: HStream b => String -> IO b
get uri = simpleHTTP (defaultGETRequest_ uri') >>= getResponseBody
  where
    uri' = fromJust $ parseURI uri

searchOEIS :: Int -> Query -> IO OEISEntries
searchOEIS n s = trim `fmap` get uri
  where
    trim = map (drop 1) . reverse . drop 2 . reverse . drop 5 . lines
    uri = oeisURL ++ "&" ++ urlEncodeVars [("n", show n), ("q", s)]

cropSeq :: Int -> String -> String
cropSeq maxLen = reverse . dropWhile (/= ',') . reverse . take maxLen

cropLine :: Int -> String -> String
cropLine maxLen s = if maxLen < length s then take (maxLen-2) s ++ ".." else s

getWidth :: IO Int
getWidth = maybe maxBound width `fmap` size

put = putStr . pack
putLn = putStrLn . pack
newline = putStrLn empty

putEntries :: Int -> OEISEntries -> IO ()
putEntries width = mapM_ $ \line ->
    case words line of
        [] -> newline
        (key:aNum:rest) -> do
            setSGR [ SetColor Foreground Dull Green ]
            put key
            setSGR [ SetColor Foreground Dull Yellow ]
            put $ ' ' : aNum
            setSGR []
            let crop = if key == "S" then cropSeq else cropLine
            put $ ' ' : crop width (unwords rest) ++ "\n"

main = do
    args <- cmdArgsRun sloane
    ncols <- getWidth
    let pick = if all args then id else select (keys args)
    let query = unwords $ terms args
    hits <- searchOEIS (limit args) query
    unless (null hits) $ do
        newline
        if url args
            then put (urls hits)
            else putEntries (ncols - 10) (pick hits)
        newline
