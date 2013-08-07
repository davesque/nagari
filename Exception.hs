data Exception e a =
    Exception e
  | Success a deriving (Show, Eq)


instance Monad (Exception e) where
    return = Success

    (Success x)   >>= f = f x
    (Exception e) >>= _ = Exception e


newtype ExceptionT e m a =
    ExceptionT { runExceptionT :: m (Exception e a) }


instance Monad m => Monad (ExceptionT e m) where
    -- | The `return` function constructs an `Exception` type value with the
    -- `Success` data constructor, then constucts an `m` type value with the
    -- function `return` from that monad's instance definition, then constucts
    -- an `ExceptionT` type value with that, in accordance with the signature
    -- of `runExceptionT`.
    return = ExceptionT . return . Success

    -- | The bind function propagates any Exception values without
    -- transformation and transforms any Success values with the function `f`.
    (ExceptionT m) >>= f = ExceptionT $
        m >>= \x -> case x of
            Success r   -> runExceptionT $ f r
            Exception l -> return $ Exception l


-- | The `throwT` function constucts an `ExceptionT` value using the
-- `Exception` data constructor to build the `Exception` type value.  It is
-- effectively the opposite of the `return` function from the instance
-- definition.
throwT :: (Monad m) => e -> ExceptionT e m a
throwT = ExceptionT . return . Exception


-- | The `catchT` function takes an `ExceptionT` type value and a function
-- which will take any exceptional values contained within the `ExceptionT`
-- type and return another `ExceptionT` which reflects whether or not the
-- exceptional state has been handled.  Any exceptional states will become
-- visible if the `Exception` data constructor is pattern matched in the case
-- statement.
catchT :: (Monad m) =>
    ExceptionT e m a ->
    (e -> ExceptionT e m a) ->
    ExceptionT e m a

catchT (ExceptionT m) f = ExceptionT $
    m >>= \x -> case x of
        Success r   -> return $ Success r
        Exception l -> runExceptionT $ f l


-- | Must explain this.  `h` stands for handle value?  What are some examples
-- of using this for things other than the IO monad?  Open yields the handle
-- value.  Close does something with the handle value and yields nothing.  Body
-- does something with the handle value and yields the result of the bracket
-- function.
bracketT :: Monad m =>
    ExceptionT e m h ->
    (h -> ExceptionT e m ()) ->
    (h -> ExceptionT e m a) ->
    ExceptionT e m a

bracketT open close body =
    open >>= \h -> ExceptionT $ do
        a <- runExceptionT (body h)
        runExceptionT (close h)
        return a
