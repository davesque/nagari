{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}

module Nagari where

----------------
-- Data types --
----------------

newtype Parser a = Parser {
    runParser :: forall r . Input
                         -- | ^ The input string to be parsed.
                         -> More
                         -- | ^ Whether or not the current parsing operation
                         -- (e.g. the status of a compound parser) is finished.
                         -> Failure r
                         -- | ^ A failure continuation (callback).  Is invoked
                         -- when the current parsing operation fails.
                         -> Success a r
                         -- | ^ A success continuation (callback).  Is invoked
                         -- when the current parsing operation succeeds.
                         -> Result r
                         -- | ^ A final result value for the parsing operation.
}

type InputParsed   = ShowS
type InputUnParsed = String
type Input         = InputUnParsed

data More = Complete
          | Incomplete deriving (Show, Eq)

type Failure r   = Input
                -- | ^ Any unparsed input left when the parsing operation
                -- failed.
                -> More
                -- | ^ Was there any more input required for the current
                -- parser?
                -> String
                -- | ^ A string describing the error that occurred.
                -> Result r
                -- | ^ A result value for the parsing operation.

type Success a r = Input
                -- | ^ Any unparsed input left when the parsing operation
                -- succeeded.
                -> More
                -- | ^ Was there any more input required for the current
                -- parser?
                -> a
                -- | ^ A result value which may become part or all of the final
                -- result value.
                -> Result r
                -- | ^ A result value for the parsing operation.

data Result r = Fail    { unParsed :: InputUnParsed, errorDesc :: String }
              | Partial { runPartial :: (InputUnParsed -> Result r) }
              | Done    { unParsed :: InputUnParsed, result :: r }

---------------
-- Instances --
---------------

instance Monad Parser where
    -- | Makes a parser that always runs the success continuation with the
    -- given value `a`.
    return a = Parser $ \input more _failK successK -> successK input more a

    -- | Constructs a new parser using a binding function `f` and the success
    -- result of a parser `p`.  The new parser receives the old failure
    -- continuation but creates a new success continuation which produces its
    -- results with the parser resulting from the binding operation.
    p >>= f = Parser $ \input more failureK successK ->
        runParser p input more failureK (\input' more' a -> runParser (f a) input' more' failureK successK)

    fail err = Parser $ \i m kf _ks -> kf i m ("Failed reading: " ++ err)

-- | Terminal failure continuation.
terminalFailureK :: Failure a
terminalFailureK i _m msg = Fail i msg

-- | Terminal success continuation.
terminalSuccessK :: Success a a
terminalSuccessK i _m a = Done i a

-- | Run a parser that cannot be resupplied via a 'Partial' result.
parseOnly :: Parser a -> String -> Either String a
parseOnly p s = case runParser p s Complete terminalFailureK terminalSuccessK of
    Fail _ err -> Left err
    Done _ a   -> Right a
    _          -> error "parseOnly: impossible error!"

-- | Support function for uncommon case of ensure.
ensure' :: Int
        -> Input
        -> More
        -> Failure r
        -> Success String r
        -> Result r
ensure' !n i m kf ks = runParser (demandInput >> go n) i m kf ks
  where go !n' = Parser $ \i' m' kf' ks' -> if   length i' >= n'
                                            then ks' i' m' i'
                                            else runParser (demandInput >> go n') i' m' kf' ks'

-- | If at least `n` elements of input are available, return the current input
-- otherwise fail.
ensure :: Int -> Parser String
ensure !n = Parser $ \i m kf ks -> if   length i >= n
                                   then ks i m i
                                   else ensure' n i m kf ks

-- | Ask for input.  If we receive any, pass it to a success continuation,
-- otherwise to a failure continuation.
prompt :: Input
       -> More
       -> (Input -> More -> Result r)
       -> (Input -> More -> Result r)
       -> Result r
prompt i _m kf ks = Partial $ \s -> if   null s
                                    then kf i Complete
                                    else ks (i ++ s) Incomplete

-- | Immediately demand more input via a 'Partial' continuation result.
demandInput :: Parser ()
demandInput = Parser $ \i m kf ks -> if   m == Complete
                                     then kf i m "not enough input"
                                     else let newFailureK i' m' = kf i' m' "not enough input"
                                              newSuccessK i' m' = ks i' m' ()
                                          in  prompt i m newFailureK newSuccessK

-- | The parser @satisfyWith f p@ transforms a character, and succeeds
-- if the predicate @p@ returns 'True' on the transformed value. The
-- parser returns the transformed character that was parsed.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  s <- ensure 1
  let c = head s
  if p c
    then let !t = tail s
         in  put t >> return c
    else fail "satisfy"

put :: String -> Parser ()
put s = Parser $ \_i m _kf ks -> ks s m ()

anyChar :: Parser Char
anyChar = satisfy $ const True

twoChars :: Parser String
twoChars = do
    a <- anyChar
    b <- anyChar
    return [a, b]
