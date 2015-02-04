-- |
-- Copyright   : Anders Claesson 2012-2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

import Data.List
import Data.Maybe
import Data.Monoid
import Data.Map ((!))
import qualified Data.Map as M
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Search as S
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Codec.Compression.GZip (decompress)
import Network.HTTP (urlEncodeVars)
import Network.Curl.Download (openURI)
import Options.Applicative
import Control.Monad
import System.Directory
import System.IO
import System.Console.ANSI
import Sloane.Types
import Sloane.Parse
import Sloane.Config
import Sloane.Transform

data Options = Options
    { longFormat     :: Bool     -- Print all fields?
    , keys           :: String   -- Keys of fields to print
    , limit          :: Int      -- Fetch at most this many entries
    , limitless      :: Bool     -- Fetch all entries (that match)
    , local          :: Bool     -- Lookup in local DB
    , url            :: Bool     -- Print URLs of found entries
    , nocolor        :: Bool     -- Do not colorize the output
    , anumber        :: Int      -- Get this sequence from the local DB
    , filtr          :: Bool     -- Filter out sequences in local DB
    , invert         :: Bool     -- Return sequences NOT in DB
    , transform      :: String   -- Apply the named transform
    , listTransforms :: Bool     -- List the names of all transforms
    , update         :: Bool     -- Updated local DB
    , version        :: Bool     -- Show version info
    , terms          :: [String] -- Search terms
    }

aNums :: Reply -> [ANum]
aNums = M.keys

grep :: PackedSeq -> SeqDB -> [PackedANum]
grep q db = mapMaybe locateANum (S.indices q db)
  where
    locateANum i = listToMaybe
        [ B.take 7 v
        | j <- [i,i-1..0]
        , B.index db j == 'A'
        , let (_,v) = B.splitAt j db
        ]

member :: PackedSeq -> SeqDB -> Bool
member q = null . grep q

invert' :: Options -> Bool -> Bool
invert' opts = if invert opts then id else not

filterSeqsIO :: Options -> SeqDB -> IO [PackedSeq]
filterSeqsIO opts db =
    filterSeqs opts db . map dropComment . B.lines <$> B.getContents
  where
    dropComment = B.takeWhile (/='#')

filterSeqs :: Options -> SeqDB -> [ByteString] -> [PackedSeq]
filterSeqs opts db = filter (\q -> invert' opts (q `member` db)) . map parseSeq

readSeqDB :: Config -> IO SeqDB
readSeqDB cfg = B.readFile (seqDBPath cfg)

readNamesDB :: Config -> IO NamesDB
readNamesDB cfg = B.readFile (namesDBPath cfg)

mkReply :: SeqMap -> NamesMap -> [ANum] -> Reply
mkReply s n = M.fromList . map (\k -> (k, M.fromList [('S', [s!k]), ('N', [n!k])]))

putReply :: Config -> Bool -> [Key] -> Reply -> IO ()
putReply cfg noColor ks reply = do
    unless (M.null reply) $ putStrLn ""
    forM_ (M.toList reply) $ \(anum, fields) -> do
        forM_ (ks `intersect` M.keys fields) $ \key -> do
            let entry = fields ! key
            forM_ entry $ \line -> do
                let line' = decodeUtf8 line
                unless noColor $ setSGR [ SetColor Foreground Dull Green ]
                putStr [key]
                unless noColor $ setSGR [ SetColor Foreground Dull Yellow ]
                putStr " " >> B.putStr (packANum anum)
                unless noColor $ setSGR []
                putStr " " >> IO.putStrLn (crop key (termWidth cfg - 10) line')
        putStrLn ""
  where
    cropText f cap s = if cap < T.length s then f cap s else s
    crop key =
      if key `elem` ['S'..'X']
        then cropText $ \cap -> T.reverse . T.dropWhile (/= ',') . T.reverse . T.take cap
        else cropText $ \cap s -> T.take (cap-2) s `T.append` T.pack ".."

keys' :: Options -> String
keys' opts = if longFormat opts then oeisKeys else keys opts

putReply' :: Options -> Config -> Reply -> IO ()
putReply' opts cfg
    | url opts  = putStr . unlines . oeisUrls cfg
    | otherwise = putReply cfg (nocolor opts) (keys' opts)

oeisUrls :: Config -> Reply -> [URL]
oeisUrls cfg = map ((oeisHost cfg ++) . B.unpack . packANum) . aNums

oeisLookup :: Int -> String -> Config -> IO Reply
oeisLookup n term cfg =
    either error parseReply <$>
    openURI (oeisURL cfg ++ "&" ++ urlEncodeVars [("n", show n), ("q", term)])

applyTransform :: Options -> String -> IO ()
applyTransform opts tname =
    case lookupTranform tname of
      Nothing -> error "No transform with that name"
      Just f  -> forM_ (terms opts) $ \input ->
                     case f $$ parseIntSeq (B.pack input) of
                       [] -> return ()
                       cs -> B.putStrLn (packIntSeq cs)

terms' :: Options -> IO Options
terms' opts
    | doNothing = return opts
    | otherwise = (\xs -> opts {terms = xs}) <$>
        case terms opts of
          [] -> isEOF >>= \b ->
                  if b then error "<stdin>: end of file"
                       else lines <$> getContents
          ts -> return ts
  where
    doNothing = version opts || anumber opts > 0 || filtr opts
              || update opts || listTransforms opts

takeUniq :: Eq a => Int -> [a] -> [a]
takeUniq n = take n . map head . group

updateDBs :: Config -> IO ()
updateDBs cfg = do
    let decomp = BL.toStrict . decompress . BL.fromStrict
    let write path = either error (B.writeFile path . decomp)
    createDirectoryIfMissing False (sloaneDir cfg)
    putStrLn $ "Downloading " ++ strippedURL cfg
    write (seqDBPath cfg) =<< openURI (strippedURL cfg)
    putStrLn $ "Downloading " ++ namesURL cfg
    write (namesDBPath cfg) =<< openURI (namesURL cfg)
    putStrLn "Done."

optionsParser :: Parser Options
optionsParser =
  abortOption ShowHelpText (long "help") <*> (Options
    <$> switch
        ( short 'l'
       <> long "long"
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
        ( short 'a'
       <> long "all"
       <> help "Fetch all matching entries (equivalent to -n 999999)" )
    <*> switch
        ( short 'q'
       <> long "quick"
       <> help ("Do a 'quick' offline search for a sequence using the local"
            ++ " database rather than oeis.org" ) )
    <*> switch
        ( long "url"
       <> help "Print URLs of found entries" )
    <*> switch
        ( long "nocolor"
       <> help "Do not colorize the output" )
    <*> option auto
        ( short 'A'
       <> metavar "NUMBER"
       <> value 0
       <> help "Fetch the sequence with this number from the local database" )
    <*> switch
        ( long "filter"
       <> help ("Read sequences from stdin and return"
            ++ " those that are in the local database") )
    <*> switch
        ( long "invert"
       <> help ("Return sequences NOT in the database;"
            ++ " only relevant when used with --filter") )
    <*> strOption
        ( short 't'
       <> long "transform"
       <> metavar "NAME"
       <> value ""
       <> help "Apply the named transform to input sequences" )
    <*> switch
        ( long "list-transforms"
       <> help "List the names of all transforms" )
    <*> switch
        ( long "update"
       <> help "Update the local database" )
    <*> switch
        ( long "version"
       <> help "Show version info" )
    <*> many (argument str (metavar "TERMS...")))

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    opts <- terms' =<< execParser (info optionsParser fullDesc)
    let n = if limitless opts then 999999 else limit opts
    let ts = terms opts
    let anum = anumber opts
    let tname = transform opts
    let lookupSeq = maybeToList . M.lookup anum
    let sloane c
         | version opts = putStrLn (nameVer c)
         | update opts = updateDBs c
         | listTransforms opts = mapM_ (putStrLn . name) transforms
         | anum > 0 = lookupSeq . parseSeqMap <$> readSeqDB c >>= mapM_ B.putStrLn
         | filtr opts = readSeqDB c >>= filterSeqsIO opts >>= mapM_ B.putStrLn
         | not (null tname) = applyTransform opts tname
         | local opts = do
             sm <- parseSeqMap <$> readSeqDB c
             nm <- parseNamesMap <$> readNamesDB c
             let nts = length ts
             forM_ ts $ \term -> do
                 let q  = parseSeq (B.pack term)
                 let q' = anchorSeq q
                 let f  = mkReply sm nm . map parseANum . takeUniq n . grep q'
                 when (nts > 1) $ putStr "seq: " >> B.putStrLn q
                 readSeqDB c >>= putReply' opts c . f
         | otherwise =
             oeisLookup n (unwords ts) c >>= putReply' opts c
    defaultConfig >>= sloane
