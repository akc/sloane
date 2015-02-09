-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module Sloane.Parse
    ( parseANum
    , parseSeq
    , parseIntSeq
    , parseNamesMap
    , parseSeqMap
    , parseReply
    , packANum
    , packIntSeq
    , anchorSeq
    ) where

import Prelude hiding (takeWhile)
import Data.Map (Map)
import qualified Data.Map as M
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Control.Monad
import Sloane.Types

type Row = (Char, ANum, ByteString)
type Rows = (ANum, Map Char [ByteString])

parse_ :: Parser t -> ByteString -> t
parse_ f t =
    let cannotParse = error "sorry, can't parse that"
    in case feed (parse f t) B.empty of
         Fail _ _ msg -> error msg
         Partial _    -> cannotParse
         Done i out   -> if B.null i then out else cannotParse

spc :: Parser Char
spc = char ' '

rest :: Parser ByteString
rest = A.takeTill isEndOfLine <* endOfLine

aNum :: Parser ANum
aNum = char 'A' >> decimal

parseANum :: ByteString -> ANum
parseANum = parse_ aNum

packANum :: ANum -> PackedANum
packANum anum = let s = show anum in B.pack ('A' : replicate (6-length s) '0' ++ s)

-- Parse an integer sequence

sep :: Char -> Parser ()
sep ' ' = void (many1 spc)
sep c   = many spc >> char c >> void (many spc)

bracketed :: Parser t -> Parser t
bracketed p = do
    c <- char '[' <|> char '{' <|> char '(' <|> (char '\'' *> char '(')
    p <* char (close c)
  where
    close '(' = ')'
    close '[' = ']'
    close '{' = '}'

intSeq0 :: Parser IntSeq
intSeq0 = do
    x <- signed decimal
    many spc
    c <- char ',' <|> char ';' <|> return ' '
    many spc
    xs <- signed decimal `sepBy` sep c
    return (x:xs)

intSeq :: Parser IntSeq
intSeq = many spc *> (bracketed intSeq0 <|> intSeq0) <* many spc

anchorSeq :: PackedSeq -> PackedSeq
anchorSeq s = B.snoc (B.cons ',' s) ','

packIntSeq :: IntSeq -> PackedSeq
packIntSeq = B.intercalate (B.pack ",") . map (B.pack . show)

parseIntSeq :: ByteString -> IntSeq
parseIntSeq = parse_ intSeq

parseSeq :: ByteString -> PackedSeq
parseSeq = parse_ (packIntSeq <$> intSeq)

-- Parsing names.gz and stripped.gz

record :: Parser (ANum, ByteString)
record = (,) <$> (aNum <* spc) <*> A.takeByteString

dropHeader :: [ByteString] -> [ByteString]
dropHeader = dropWhile (\line -> B.head line == '#')

parseRecord :: ByteString -> (ANum, ByteString)
parseRecord = parse_ record

parseRecords :: ByteString -> Map ANum ByteString
parseRecords = M.fromList . map parseRecord . dropHeader . B.lines

parseNamesMap :: DB Names -> NamesMap
parseNamesMap (DB bs) = parseRecords bs

-- XXX: The triming here makes a noticeable slowdown. This should be
-- fixable with laziness.
parseSeqMap :: DB Seq -> SeqMap
parseSeqMap (DB bs) = M.map (B.init . B.tail) (parseRecords bs)

-- Parsing a reply from oeis.org/search?fmt=text&n=?&q=?

row :: Parser Row
row = (,,) <$> (char '%' *> anyChar)
           <*> (spc *> aNum)
           <*> ((spc *> rest) <|> (endOfLine *> return B.empty))

rows :: Parser Rows
rows = mkMap1 <$> many1 row
  where
    mkMap2 = M.fromListWith (flip (++))
    mkMap1 rs@((_,a,_):_) = (a, mkMap2 (map (\(key, _, r) -> (key, [r])) rs))

noise :: Parser ()
noise = skipMany (notChar '%')

reply :: Parser Reply
reply = (M.fromList . concat) <$>
        (noise *> (many1 rows `sepBy` many1 endOfLine) <* noise)

parseReply :: ByteString -> Reply
parseReply = parse_ reply
