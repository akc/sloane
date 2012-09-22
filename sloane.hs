{-# LANGUAGE DeriveDataTypeable #-}

import Prelude hiding (all)
import System.Console.CmdArgs
import Data.Maybe (fromJust)
import Control.Monad (when)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Network.URL (importURL, exportURL, add_param)

type OEISEntries = String
type Query = String
type Key = Char

oeisURL = fromJust $ importURL "http://oeis.org/search?fmt=text"
oeisKeys = "ISTUVWXNDHFYAOEeptoKC"

data Sloane = Sloane { keys  :: String
                     , all   :: Bool
                     , limit :: Int
                     , terms :: String
                     }
              deriving (Data, Typeable)

sloane = cmdArgsMode $ Sloane 
         { keys  = "SN"  &= typ "KEYS"
                &= help "Keys of fields to print, http://oeis.org/eishelp1.html (default: SN)"
         , all   = False &= help "Print all fields"
         , limit = 5     &= name "n"  &= help "Limit the number of entries retrieved (default: 5)"
         , terms = def   &= argPos 0  &= typ "SEARCH-TERMS"
         }
         &= versionArg [summary "sloane 0.1.3"]
         &= summary "Search Sloane's On-Line Encyclopedia of Integer Sequences"

select :: [Key] -> OEISEntries -> OEISEntries
select ks = unlines . filter (\xs -> null xs || head xs `elem` ks) . lines

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
    putStrLn $ if all args then result else select (keys args) result
