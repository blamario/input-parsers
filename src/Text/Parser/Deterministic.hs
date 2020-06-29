{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Deterministic parsers can be restricted to a single parsing result.

module Text.Parser.Deterministic where

import Control.Applicative (Applicative ((<*>), pure), Alternative ((<|>), many, some), liftA2, optional)
import Control.Arrow (first)
import Control.Monad (MonadPlus, void)
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.Reader (ReaderT(..), mapReaderT)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy (WriterT(WriterT))
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT(WriterT))
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT(StateT))
import qualified Control.Monad.Trans.State.Strict as Strict (StateT(StateT))
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

import Text.Parser.Internal (mapLazyWriterT, mapStrictWriterT, mapLazyStateT, mapStrictStateT)
import Text.Parser.Wrapper (Lazy(..), Strict(..))

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

#ifdef MIN_VERSION_binary
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy

import qualified Data.Binary.Get as Binary
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

instance (Monad m, DeterministicParsing m) => DeterministicParsing (IdentityT m) where
  IdentityT p <<|> IdentityT q = IdentityT (p <<|> q)
  takeOptional (IdentityT p) = IdentityT (takeOptional p)
  takeMany (IdentityT p) = IdentityT (takeMany p)
  takeSome (IdentityT p) = IdentityT (takeSome p)
  concatAll (IdentityT p) = IdentityT (concatAll p)
  skipAll (IdentityT p) = IdentityT (skipAll p)

instance (MonadPlus m, DeterministicParsing m) => DeterministicParsing (ReaderT e m) where
  ReaderT p <<|> ReaderT q = ReaderT (\a-> p a <<|> q a)
  takeOptional = mapReaderT takeOptional
  takeMany = mapReaderT takeMany
  takeSome = mapReaderT takeSome
  concatAll = mapReaderT concatAll
  skipAll = mapReaderT skipAll

instance (MonadPlus m, DeterministicParsing m, Monoid w) => DeterministicParsing (Lazy.WriterT w m) where
  Lazy.WriterT p <<|> Lazy.WriterT q = Lazy.WriterT (p <<|> q)
  takeOptional = mapLazyWriterT takeOptional
  takeMany = mapLazyWriterT takeMany
  takeSome = mapLazyWriterT takeSome
  concatAll = mapLazyWriterT concatAll
  skipAll = mapLazyWriterT skipAll

instance (MonadPlus m, DeterministicParsing m, Monoid w) => DeterministicParsing (Strict.WriterT w m) where
  Strict.WriterT p <<|> Strict.WriterT q = Strict.WriterT (p <<|> q)
  takeOptional = mapStrictWriterT takeOptional
  takeMany = mapStrictWriterT takeMany
  takeSome = mapStrictWriterT takeSome
  concatAll = mapStrictWriterT concatAll
  skipAll = mapStrictWriterT skipAll

instance (MonadPlus m, DeterministicParsing m, Monoid w) => DeterministicParsing (Lazy.StateT w m) where
  Lazy.StateT p <<|> Lazy.StateT q = Lazy.StateT (\a-> p a <<|> q a)
  takeOptional = mapLazyStateT takeOptional
  takeMany = mapLazyStateT takeMany
  takeSome = mapLazyStateT takeSome
  concatAll = mapLazyStateT concatAll
  skipAll = mapLazyStateT skipAll

instance (MonadPlus m, DeterministicParsing m, Monoid w) => DeterministicParsing (Strict.StateT w m) where
  Strict.StateT p <<|> Strict.StateT q = Strict.StateT (\a-> p a <<|> q a)
  takeOptional = mapStrictStateT takeOptional
  takeMany = mapStrictStateT takeMany
  takeSome = mapStrictStateT takeSome
  concatAll = mapStrictStateT concatAll
  skipAll = mapStrictStateT skipAll

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

#ifdef MIN_VERSION_binary
instance DeterministicParsing (Lazy Binary.Get) where
  (<<|>) = (<|>)
  takeOptional = optional
  takeMany = many
  takeSome = some

instance DeterministicParsing (Strict Binary.Get) where
  (<<|>) = (<|>)
  takeOptional = optional
  takeMany = many
  takeSome = some
#endif
