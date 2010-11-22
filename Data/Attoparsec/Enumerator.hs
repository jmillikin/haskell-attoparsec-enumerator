-----------------------------------------------------------------------------
-- |
-- Module: Data.Attoparsec.Enumerator
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Attoparsec.Enumerator
	( ParseError (..)
	, iterParser
	) where
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B
import qualified Data.Enumerator as E

-- | The context and message from a 'A.Fail' value.
data ParseError = ParseError
	{ errorContexts :: [String]
	, errorMessage :: String
	}
	deriving (Show, Typeable)

instance Exception ParseError

-- | Convert an Attoparsec 'A.Parser' into an 'E.Iteratee'. The parser will
-- be streamed bytes until it returns 'A.Done' or 'A.Fail'.
--
-- If parsing fails, the iteratee's error value will contain a 'ParseError'.
iterParser :: Monad m => A.Parser a -> E.Iteratee B.ByteString m a
iterParser p = E.continue (step (A.parse p)) where
	step parse (E.Chunks xs) = parseLoop parse xs
	step parse E.EOF = case A.feed (parse B.empty) B.empty of
		A.Done extra a -> E.yield a $ if B.null extra
			then E.Chunks []
			else E.Chunks [extra]
		A.Partial _ -> err [] "iterParser: divergent parser"
		A.Fail _ ctx msg -> err ctx msg
	
	parseLoop parse [] = E.continue (step parse)
	parseLoop parse (x:xs) = case parse x of
		A.Done extra a -> E.yield a $ if B.null extra
			then E.Chunks xs
			else E.Chunks (extra:xs)
		A.Partial parse' -> parseLoop parse' xs
		A.Fail _ ctx msg -> err ctx msg
	
	err ctx msg = E.throwError (ParseError ctx msg)
