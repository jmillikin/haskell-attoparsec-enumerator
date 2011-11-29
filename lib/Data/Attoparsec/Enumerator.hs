{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module: Data.Attoparsec.Enumerator
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
module Data.Attoparsec.Enumerator
	( ParseError (..)
	, AttoparsecInput
	, iterParser
	) where

import           Control.Exception (Exception)
import           Data.Typeable (Typeable)
import qualified Data.ByteString as B
import qualified Data.Text as T

import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text
import qualified Data.Attoparsec.Types as A
import qualified Data.Enumerator as E

-- | The context and message from a 'A.Fail' value.
data ParseError = ParseError
	{ errorContexts :: [String]
	, errorMessage :: String
	}
	deriving (Show, Typeable)

instance Exception ParseError

-- | A class of types which may be consumed by an Attoparsec parser.
--
-- Since: 0.3
class AttoparsecInput a where
	parseA :: A.Parser a b -> a -> A.IResult a b
	feedA :: A.IResult a b -> a -> A.IResult a b
	empty :: a
	isNull :: a -> Bool
	notEmpty :: [a] -> [a]

instance AttoparsecInput B.ByteString where
	parseA = Data.Attoparsec.ByteString.parse
	feedA = Data.Attoparsec.ByteString.feed
	empty = B.empty
	isNull = B.null
	notEmpty = filter (not . B.null)

instance AttoparsecInput T.Text where
	parseA = Data.Attoparsec.Text.parse
	feedA = Data.Attoparsec.Text.feed
	empty = T.empty
	isNull = T.null
	notEmpty = filter (not . T.null)

-- | Convert an Attoparsec 'A.Parser' into an 'E.Iteratee'. The parser will
-- be streamed bytes until it returns 'A.Done' or 'A.Fail'.
--
-- If parsing fails, a 'ParseError' will be thrown with 'E.throwError'. Use
-- 'E.catchError' to catch it.
iterParser :: (AttoparsecInput a, Monad m) => A.Parser a b -> E.Iteratee a m b
iterParser p = E.continue (step (parseA p)) where
	step parse (E.Chunks xs) = parseLoop parse (notEmpty xs)
	step parse E.EOF = case feedA (parse empty) empty of
		A.Done _ b -> E.yield b E.EOF
		A.Partial _ -> err [] "iterParser: divergent parser"
		A.Fail _ ctx msg -> err ctx msg
	
	parseLoop parse [] = E.continue (step parse)
	parseLoop parse (x:xs) = case parse x of
		A.Done extra a -> E.yield a $ if isNull extra
			then E.Chunks xs
			else E.Chunks (extra:xs)
		A.Partial parse' -> parseLoop parse' xs
		A.Fail _ ctx msg -> err ctx msg
	
	err ctx msg = E.throwError (ParseError ctx msg)
