import Control.Monad
import Data.Monoid

newtype Writer w a = Writer { runWriter :: (a, w) } deriving (Show)

-- | Bare bones writer monad.
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)

    Writer (x, l) >>= f =
        let (x', l') = runWriter $ f x
        in Writer (x', l `mappend` l')

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

-- | Builds a `WriterT` data constructor from a (result, log) tuple.
writer :: (Monad m) => (a, w) -> WriterT w m a
writer = WriterT . return

-- | Don't see a reason to involve the `Writer` monad in this one.  It only
-- seems to complicate the code.
instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return x = writer (x, mempty)

    m >>= f = WriterT $ do
        (x, l)   <- runWriterT m
        (x', l') <- runWriterT $ f x
        return (x', l `mappend` l')

execWriterT :: (Monad m) => WriterT w m a -> m w
execWriterT (WriterT m) = liftM snd m

mapWriterT :: (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f (WriterT m) = WriterT $ f m

tell :: (Monad m) => w -> WriterT w m ()
tell x = writer ((), x)

logValue :: (Show a, Monad m) => a -> WriterT [String] m a
logValue x = writer (x, ["Got number: " ++ show x])

myLog :: WriterT [String] Maybe Int
myLog = do
    a <- logValue 10
    tell ["Just going to return this"]
    mapWriterT (\m -> do
        (x, l) <- m
        return (2 * x, l `mappend` ["blah"])
      ) $ return a
