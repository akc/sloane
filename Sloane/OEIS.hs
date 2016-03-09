{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Copyright   : Anders Claesson 2015, 2016
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module Sloane.OEIS
    (
    -- * Types
      URL
    , ANum (..)
    , PackedSeq (..)
    , OEISEntry (..)
    -- * Parse names.gz and stripped.gz
    , parseNames
    , parseStripped
    , parseTermsErr
    , parseTermsOfRecords
    -- * Parse replies from oeis.org
    , parseOEISEntries
    -- * Parse sequences
    , shave
    , parseIntegerSeq
    , packSeq
    -- * Parse A-numbers and B-numbers
    , parseANum
    ) where

import GHC.Generics (Generic)
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Map (Map)
import qualified Data.Map as M
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Aeson
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8
import Control.Monad
import Control.Applicative
import Sloane.Utils
import Sloane.Entry

-- | An OEIS key is `Char`.
type Key = Char

type Row = (Key, ANum, ByteString)

-- | A URL is currently just a synonym for `String`.
type URL = String

-- | An A-number is the character \'A\' followed by a six digit
-- number. Here we represent that by a wrapped (7 character)
-- `ByteString`.
newtype ANum = ANum {unANum :: ByteString} deriving (Eq, Ord, Show, Generic)

instance ToJSON ANum where
    toJSON (ANum bs) = String (decodeUtf8 bs)

instance FromJSON ANum where
    parseJSON (String s) = pure $ ANum (encodeUtf8 s)
    parseJSON _ = mzero

-- | A `PackedSeq` is a wrapped `ByteString`.
newtype PackedSeq = PSeq {unPSeq :: ByteString} deriving (Eq, Show, Generic)

instance Monoid PackedSeq where
    mempty = PSeq mempty
    mappend (PSeq x) (PSeq y) = PSeq (mappend x y)

instance IsString PackedSeq where
    fromString = PSeq . fromString

instance ToJSON PackedSeq where
    toJSON (PSeq bs) = String (decodeUtf8 bs)

instance FromJSON PackedSeq where
    parseJSON (String s) = pure $ PSeq (encodeUtf8 s)
    parseJSON _ = mzero

data OEISEntry = OEISEntry ANum (Map Key [ByteString]) deriving Show

instance ToJSON OEISEntry where
    toJSON (OEISEntry anum tbl) =
        object ("A-number" .= toJSON anum :
                [ T.singleton key .= toJSON (map decodeUtf8 ls)
                | (key, ls) <- M.toList tbl
                ])

instance FromJSON OEISEntry where
    parseJSON (Object v) =
        let f k = (,) <$> pure k <*> (map encodeUtf8 <$> v .: T.singleton k)
        in OEISEntry <$> (v .: "A-number") <*> (M.fromList <$> mapM f oeisKeys)
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
parseNames :: ByteString -> [(ANum, Name)]
parseNames bs = [ (a, Name n) | (a, n) <- parseRecords bs ]

-- | Parse a list of A-number-sequence pairs. It's purpose is to parse
-- lines of the @stripped@ file. A typical line of that file looks like
-- this:
--
-- > A000108 ,1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,
--
parseStripped :: ByteString -> [(ANum, [Integer])]
parseStripped bs = [ (anum, parseIntegerSeqErr (shave s)) | (anum, s) <- parseRecords bs ]

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

noise :: Parser ()
noise = skipMany (notChar '%')

oeisEntry :: Parser OEISEntry
oeisEntry = mkMap1 <$> many1 row
  where
    mkMap2 = M.fromListWith (flip (++))
    mkMap1 rs@((_,a,_):_) = OEISEntry a $ mkMap2 (map (\(key, _, r) -> (key, [r])) rs)
    mkMap1 [] = error "internal error"

oeisEntries :: Parser [OEISEntry]
oeisEntries = concat <$> (noise *> (many1 oeisEntry `sepBy` many1 endOfLine) <* noise)

parseOEISEntries :: ByteString -> [OEISEntry]
parseOEISEntries = fromMaybe [] . parse_ oeisEntries

-------------------------------------------------------------------------------
-- Parse sequences
-------------------------------------------------------------------------------

integerSeq :: Parser [Integer]
integerSeq = signed decimal `sepBy` char ','

-- | Parse a sequence of `Integer`s.
parseIntegerSeq :: ByteString -> Maybe [Integer]
parseIntegerSeq = parse_ (integerSeq <* endOfInput) . B.filter (/=' ')

-- | Parse a sequence of `Integer`s or throw an error.
parseIntegerSeqErr :: ByteString -> [Integer]
parseIntegerSeqErr = fromMaybe (error "error parsing sequence") . parseIntegerSeq

-- | Pack a sequence of `Integers`s into a `PackedSeq`. E.g.
--
-- > packSeq [1,-1,3] = PSeq {unPSeq = "1,-1,3"}
--
packSeq :: [Integer] -> PackedSeq
packSeq = PSeq . B.intercalate (B.pack ",") . map (B.pack . show)

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

-- | Pack an A-number given as an `Int` into a wrapped `ByteString`
-- consistsing of an \'A\' followed by six digits. E.g.
--
-- > packANum 1234 = ANum {unANum = "A001234"}
--
packANum :: Int -> ANum
packANum anum = ANum $ B.cons 'A' (pad 6 anum)

-- | Run the `aNumInt` parser.
parseANum :: ByteString -> Maybe ANum
parseANum = parse_ (packANum <$> aNumInt)
