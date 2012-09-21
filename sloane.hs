{-# LANGUAGE DeriveDataTypeable #-}

import Prelude hiding (all)
import System.Console.CmdArgs
import Data.List (groupBy)
import Data.Maybe (fromJust)
import Control.Monad (when)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Network.URL (importURL, exportURL, add_param)

type OEISEntry = String
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
         &= versionArg [summary "sloane 0.1.2"]
         &= summary "Search Sloane's On-Line Encyclopedia of Integer Sequences"

nonempty = not . null

parse :: String -> [OEISEntry]
parse = map (unlines . filter nonempty) . groupBy (\_ x -> nonempty x)
        . map (drop 1) . reverse . drop 2 . reverse . drop 5 . lines

select :: [Key] -> OEISEntry -> OEISEntry
select ks = unlines . filter (\(k:_) -> k `elem` ks) . lines

searchOEIS :: Int -> Query -> IO [OEISEntry]
searchOEIS n s = do
  parse `fmap` (simpleHTTP (getRequest url) >>= getResponseBody)
    where
      url = exportURL $ oeisURL `add_param` ("n", show n) `add_param` ("q", s)

main = do
  args <- cmdArgsRun sloane
  es <- searchOEIS (limit args) . filter (`notElem` "[]") $ terms args
  when (nonempty es) $ putStrLn ""
  let ks = if all args then oeisKeys else keys args
  mapM_ (putStrLn . select ks) es
