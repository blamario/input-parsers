module Text.Parser.Internal where

import Control.Applicative (Applicative, liftA2)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy (WriterT(WriterT))
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT(WriterT))
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT(StateT))
import qualified Control.Monad.Trans.State.Strict as Strict (StateT(StateT))

mapLazyWriterT :: Applicative m => (m a -> m b) -> Lazy.WriterT w m a -> Lazy.WriterT w m b
mapLazyWriterT f (Lazy.WriterT p) = Lazy.WriterT (apply p)
   where apply m = liftA2 (,) (f $ fst <$> m) (snd <$> m)

mapStrictWriterT :: Applicative m => (m a -> m b) -> Strict.WriterT w m a -> Strict.WriterT w m b
mapStrictWriterT f (Strict.WriterT p) = Strict.WriterT (apply p)
   where apply m = liftA2 (,) (f $ fst <$> m) (snd <$> m)

mapLazyStateT :: Applicative m => (m a -> m b) -> Lazy.StateT w m a -> Lazy.StateT w m b
mapLazyStateT f (Lazy.StateT p) = Lazy.StateT (apply . p)
   where apply m = liftA2 (,) (f $ fst <$> m) (snd <$> m)

mapStrictStateT :: Applicative m => (m a -> m b) -> Strict.StateT w m a -> Strict.StateT w m b
mapStrictStateT f (Strict.StateT p) = Strict.StateT (apply . p)
   where apply m = liftA2 (,) (f $ fst <$> m) (snd <$> m)
