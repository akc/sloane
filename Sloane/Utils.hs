{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright   : Anders Claesson
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module Sloane.Utils
    ( pad
    , parens
    , oneOf
    , chainl1
    , parse_
    ) where

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8
import Control.Applicative


-- | @pad d n@ packs the integer @n@ into a `ByteString` padding with
-- \'0\' on the right to achieve length @d@.
--
-- > pad 6 123 = "000123"
--
pad :: Int -> Int -> ByteString
pad d n = B.replicate (d - B.length s) '0' <> s where s = B.pack (show n)

-- | Parse @p@ enclosed in parenthesis.
parens :: Parser a -> Parser a
parens p = string "(" *> p <* string ")"

-- | Parse one of the words in the given `ByteString`.
oneOf :: ByteString -> Parser ByteString
oneOf s = choice (map string (B.words s))

-- | Parse one or more occurrences of @p@, separated by an operator.
-- Returns a value obtained by a left associative application.
chainl1 :: (Monad f, Alternative f) => f a -> f (a -> a -> a) -> f a
chainl1 p op = p >>= rest
  where
    rest a = do { f <- op; b <- p; rest $! f a b } <|> pure a

-- | Run a parser.
parse_ :: Parser t -> ByteString -> Maybe t
parse_ f = either (const Nothing) Just . parseOnly f
