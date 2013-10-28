{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Copyright   : Anders Claesson 2012, 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

import Prelude hiding (all)
import System.Console.CmdArgs
import Data.Maybe (fromJust)
import Control.Monad (when, guard)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Network.URL (importURL, exportURL, add_param)

type OEISEntries = String
type Query = String
type Key = Char

oeisHost = "http://oeis.org"
oeisURL  = fromJust . importURL $ oeisHost ++ "/search?fmt=text"
oeisKeys = "ISTUVWXNDHFYAOEeptoKC"

data Sloane = Sloane { keys  :: String
                     , all   :: Bool
                     , url   :: Bool
                     , limit :: Int
                     , terms :: String
                     }
              deriving (Data, Typeable)

sloane = cmdArgsMode $ Sloane 
         { keys  = "SN"  &= typ "KEYS" &= help "Keys of fields to print (default: SN)"
         , all   = False &= name "a"   &= help "Print all fields"
         , url   = False &= name "u"   &= help "Print urls of found entries"
         , limit = 5     &= name "n"   &= help "Limit the number of entries retrieved (default: 5)"
         , terms = def   &= argPos 0   &= typ "SEARCH-TERMS"
         }
         &= versionArg [summary "sloane 0.1.5"]
         &= summary "Search Sloane's On-Line Encyclopedia of Integer Sequences"

select :: [Key] -> OEISEntries -> OEISEntries
select ks = unlines . filter (\xs -> null xs || head xs `elem` ks) . lines

urls :: OEISEntries -> String
urls es = unlines $ do
            ids <- lines $ select "I" es
            guard $ not (null ids)
            let aNum = (words ids) !! 1
            return $ oeisHost ++ "/" ++ aNum

searchOEIS :: Int -> Query -> IO OEISEntries
searchOEIS n s = do
  trim `fmap` (simpleHTTP (getRequest url) >>= getResponseBody)
    where
      trim = unlines . map (drop 1) . reverse . drop 2 . reverse . drop 5 . lines
      url = exportURL $ oeisURL `add_param` ("n", show n) `add_param` ("q", s)

main = do
  args <- cmdArgsRun sloane
  result <- searchOEIS (limit args) . filter (`notElem` "[]") $ terms args
  when (not $ null result) $ do
    putStrLn ""
    if (url args)
      then
        putStrLn $ urls result
      else
        putStrLn $ if all args then result else select (keys args) result
