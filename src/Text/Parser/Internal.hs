module Text.Parser.Internal where

import Control.Applicative (Applicative, liftA2)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict

mapLazyWriterT :: Applicative m => (m a -> m b) -> Lazy.WriterT w m a -> Lazy.WriterT w m b
mapLazyWriterT f (Lazy.WriterT p) = Lazy.WriterT (apply p)
   where apply m = liftA2 (,) (f $ fst <$> m) (snd <$> m)

mapStrictWriterT :: Applicative m => (m a -> m b) -> Strict.WriterT w m a -> Strict.WriterT w m b
mapStrictWriterT f (Strict.WriterT p) = Strict.WriterT (apply p)
   where apply m = liftA2 (,) (f $ fst <$> m) (snd <$> m)
