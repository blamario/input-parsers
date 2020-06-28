{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Deterministic parsers can be restricted to a single parsing result.

module Text.Parser.Deterministic where

import Control.Applicative (Applicative ((<*>), pure), Alternative ((<|>), many, some), liftA2, optional)
import Control.Monad (void)
import Data.Functor ((<$>))
import qualified Data.List as List
import Data.Monoid (Monoid, mappend, mempty)
import Data.String (IsString (fromString))
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

import Text.Parser.Char (CharParsing)
import Text.Parser.Combinators (Parsing, count, eof, notFollowedBy, try, unexpected)
import Text.Parser.LookAhead (LookAheadParsing, lookAhead)
import qualified Text.Parser.Char as Char

#ifdef MIN_VERSION_attoparsec
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Text as Text

import qualified Data.Attoparsec.ByteString as Attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec.Char8
import qualified Data.Attoparsec.Text as Attoparsec.Text
#endif

-- | Combinator methods for constructing deterministic parsers, /i.e./, parsers that can succeed with only a single
-- result.
class Parsing m => DeterministicParsing m where
   -- | Left-biased choice: if the left alternative succeeds, the right one is never tried.
   infixl 3 <<|>
   (<<|>) :: m a -> m a -> m a
   -- | Like 'optional', but never succeeds with @Nothing@ if the argument parser can succeed.
   takeOptional :: m a -> m (Maybe a)
   -- | Like 'some', but always consuming the longest matching sequence of input.
   takeMany :: m a -> m [a]
   -- | Like 'some', but always consuming the longest matching sequence of input.
   takeSome :: m a -> m [a]
   -- | Like 'Text.Parser.Input.concatMany', but always consuming the longest matching sequence of input.
   concatAll :: Monoid a => m a -> m a
   -- | Like 'Text.Parser.Combinators.skipMany', but always consuming the longest matching sequence of input.
   skipAll :: m a -> m ()

   p <<|> q = try p <|> notFollowedBy (void p) *> q
   takeOptional p = Just <$> p <<|> pure Nothing
   takeMany p = many p <* notFollowedBy (void p)
   takeSome p = some p <* notFollowedBy (void p)
   concatAll p = go
      where go = liftA2 mappend p go <<|> pure mempty
   skipAll p = p *> skipAll p <<|> pure ()

instance DeterministicParsing ReadP where
  (<<|>) = (ReadP.<++)

#ifdef MIN_VERSION_attoparsec
instance DeterministicParsing Attoparsec.Parser where
  (<<|>) = (<|>)
  takeOptional = optional
  takeMany = many
  takeSome = some
  skipAll = Attoparsec.skipMany

instance DeterministicParsing Attoparsec.Text.Parser where
  (<<|>) = (<|>)
  takeOptional = optional
  takeMany = many
  takeSome = some
  skipAll = Attoparsec.Text.skipMany
#endif
