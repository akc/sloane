{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module Sloane.OEIS
    (
    -- * Types
      URL
    , Key
    , Name
    , ANum (..)
    , PackedSeq (..)
    , Table (..)
    , Reply (..)
    -- * Parse names.gz and stripped.gz
    , parseNames
    , parseStripped
    , parseTermsErr
    , parseTermsOfRecords
    -- * Parse replies from oeis.org
    , oeisKeys
    , parseReplies
    -- * Parse sequences
    , shave
    , parseSeqErr
    , parseIntegerSeq
    , packedSeq
    , packSeq
    -- * Parse A-numbers and B-numbers
    , aNumInt
    , parseANum
    , packANum
    , tag
    ) where

import GHC.Generics (Generic)
import Data.Maybe
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Data.String
import Data.Ratio
import Data.Map (Map)
import qualified Data.Map as M
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Aeson
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as Ch
import Data.Attoparsec.ByteString.Char8
import Control.DeepSeq
import Control.Monad
import Control.Applicative
import Sloane.Utils

-- | An OEIS key is `Char`.
type Key  = Char

-- | The name of an OEIS entry is a short description of the
-- sequence. Here represented as a `ByteString`.
type Name = ByteString

type Row  = (Key, ANum, ByteString)

-- | A URL is currently just a synonym for `String`.
type URL  = String

-- | An A-number is the character \'A\' followed by a six digit
-- number. Here we represent that by a wrapped (7 character)
-- `ByteString`.
newtype ANum = ANum {unANum :: ByteString} deriving (Eq, Ord, Show, Generic)

-- | A `PackedSeq` is a wrapped `ByteString`.
newtype PackedSeq = PSeq {unPSeq :: ByteString} deriving (Eq, Show, Generic)

instance NFData PackedSeq

instance Monoid PackedSeq where
    mempty = PSeq mempty
    mappend (PSeq x) (PSeq y) = PSeq (mappend x y)

instance IsString PackedSeq where
    fromString = PSeq . fromString

-- | A `Table` represents an OEIS entry. It is a `Map` from OEIS keys to
-- lists of `ByteString`s.
newtype Table = Table (Map Key [ByteString]) deriving Show

-- | A `Reply` is an A-number together with an associated `Table` (OEIS
-- entry).
data Reply = Reply ANum Table deriving Show

instance ToJSON ANum where
    toJSON (ANum bs) = String (decodeUtf8 bs)

instance FromJSON ANum where
    parseJSON (String s) = pure $ ANum (encodeUtf8 s)
    parseJSON _ = mzero

instance ToJSON PackedSeq where
    toJSON (PSeq bs) = String (decodeUtf8 bs)

instance FromJSON PackedSeq where
    parseJSON (String s) = pure $ PSeq (encodeUtf8 s)
    parseJSON _ = mzero

instance ToJSON Table where
    toJSON (Table tbl) =
        object [ T.singleton key .= toJSON (map decodeUtf8 ls)
               | (key, ls) <- M.toList tbl
               ]

instance ToJSON Reply where
    toJSON (Reply anum table) =
        object [ "A-number" .= toJSON anum
               , "table" .= toJSON table
               ]

instance FromJSON Table where
    parseJSON (Object v) =
        let f k = (,) <$> pure k <*> (map encodeUtf8 <$> v .: T.singleton k)
        in Table . M.fromList <$> mapM f oeisKeys
    parseJSON _ = mzero

instance FromJSON Reply where
    parseJSON (Object v) = Reply <$> v .: "A-number" <*> v .: "table"
    parseJSON _ = mzero

spc :: Parser Char
spc = char ' '

aNum :: Parser ANum
aNum = ANum <$> (B.cons <$> char 'A' <*> takeWhile1 isDigit)

-------------------------------------------------------------------------------
-- Parsing names.gz and stripped.gz
-------------------------------------------------------------------------------

dropHeader :: [ByteString] -> [ByteString]
dropHeader = dropWhile (\line -> B.head line == '#')

parseRecords :: ByteString -> [(ANum, ByteString)]
parseRecords = mapMaybe (parse_ record) . dropHeader . B.lines
  where
    record = (,) <$> (aNum <* spc) <*> A.takeByteString

-- | Parse a list of A-number-names pairs and build a `Map` from the
-- result. It's purpose is to parse lines of the @names@ file. A typical
-- line of that file looks like this:
--
-- > A000108 Catalan numbers: C(n) = binomial(2n,n)/(n+1) = (2n)!/(n!(n+1)!).
--
parseNames :: ByteString -> [(ANum, ByteString)]
parseNames = parseRecords

-- | Parse a list of A-number-sequence pairs. It's purpose is to parse
-- lines of the @stripped@ file. A typical line of that file looks like
-- this:
--
-- > A000108 ,1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,
--
parseStripped :: ByteString -> [(ANum, PackedSeq)]
parseStripped bs = [ (anum, PSeq (shave s)) | (anum, s) <- parseRecords bs ]

-------------------------------------------------------------------------------
-- Building the Bloom filter
-------------------------------------------------------------------------------

terms :: Parser [ByteString]
terms = takeWhile1 (/=',') `sepBy` char ','

-- | Extract the list of terms from a `ByteString` of comma separated
-- terms.
parseTermsErr :: ByteString -> [ByteString]
parseTermsErr = fromMaybe (error "error parsing terms") . parse_ terms

-- | The purpose of this function is extract all terms from the records
-- (lines) of the @stripped@ file (skipping over A-numbers).
parseTermsOfRecords :: ByteString -> [ByteString]
parseTermsOfRecords =
      concat
    . mapMaybe (parse_ ((aNum <* spc) *> char ',' *> terms))
    . dropHeader . B.lines

-------------------------------------------------------------------------------
-- Parsing replies from oeis.org/search?fmt=text
-------------------------------------------------------------------------------

-- | The complete list of keys used by OEIS (<http://oeis.org/eishelp2.html>).
oeisKeys :: String
oeisKeys = "ISTUVWXNDHFYAOEeptoKC"

row :: Parser Row
row = (,,)
    <$> (char '%' *> anyChar)
    <*> (spc *> aNum)
    <*> ((spc *> rest) <|> (endOfLine *> return B.empty))
  where
    rest = A.takeTill isEndOfLine <* endOfLine

rows :: Parser Reply
rows = mkMap1 <$> many1 row
  where
    mkMap2 = Table . M.fromListWith (flip (++))
    mkMap1 rs@((_,a,_):_) = Reply a $ mkMap2 (map (\(key, _, r) -> (key, [r])) rs)
    mkMap1 [] = error "internal error"

noise :: Parser ()
noise = skipMany (notChar '%')

replies :: Parser [Reply]
replies = concat <$> (noise *> (many1 rows `sepBy` many1 endOfLine) <* noise)

-- | Parse OEIS replies as recieved from @oeis.org/search?fmt=text@.
parseReplies :: ByteString -> [Reply]
parseReplies = fromMaybe [] . parse_ replies

-------------------------------------------------------------------------------
-- Parse sequences
-------------------------------------------------------------------------------

rat :: Parser Rational
rat = (%) <$> signed decimal <*> ((char '/' *> decimal) <|> return 1)

ratSeq :: Parser [Rational]
ratSeq = rat `sepBy` char ','

integerSeq :: Parser [Integer]
integerSeq = signed decimal `sepBy` char ','

parseSeq :: ByteString -> Maybe [Rational]
parseSeq = parse_ (ratSeq <* endOfInput) . B.filter (/=' ')

-- | Parse a sequence of `Rational`s.
parseSeqErr :: ByteString -> [Rational]
parseSeqErr = fromMaybe (error "error parsing sequence") . parseSeq

-- | Parse a sequence of `Integer`s.
parseIntegerSeq :: ByteString -> Maybe [Integer]
parseIntegerSeq = parse_ (integerSeq <* endOfInput) . B.filter (/=' ')

-- | Parser for `PackedSeq`.
packedSeq :: Parser PackedSeq
packedSeq = PSeq <$> (char '{' *> Ch.takeWhile (/='}') <* char '}')

-- | Pack a sequence of `Rational`s into a `PackedSeq`. E.g.
--
-- > packSeq [1,1/2,1/3] = PSeq {unPSeq = "1,1/2,1/3"}
--
packSeq :: [Rational] -> PackedSeq
packSeq = PSeq . B.intercalate (B.pack ",") . map (B.pack . f)
  where
    f r = case (numerator r, denominator r) of
            (n, 1) -> show n
            (n, d) -> show n ++ '/':show d

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

-- | \"Shave\" off the first and last element of a `ByteString`. E.g.
--
-- > shave "{1,2,3}" = "1,2,3"
--
shave :: ByteString -> ByteString
shave = B.init . B.tail

-- | A parser for A-numbers as `Int`s.
aNumInt :: Parser Int
aNumInt = char 'A' >> decimal

-- | Run the `aNumInt` parser.
parseANum :: ByteString -> Maybe ANum
parseANum = parse_ (packANum <$> aNumInt)

-- | Pack an A-number given as an `Int` into a wrapped `ByteString`
-- consistsing of an \'A\' followed by six digits. E.g.
--
-- > packANum 1234 = ANum {unANum = "A001234"}
--
packANum :: Int -> ANum
packANum anum = ANum $ B.cons 'A' (pad 6 anum)

-- | A parser for tags (B-numbers) as `Int`s.
tag :: Parser Int
tag = string "TAG" >> decimal
