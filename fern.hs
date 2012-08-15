import Prelude hiding (lookup)
import Data.Map (insert, fromList, lookup)

import ParseFern

----------------------------
-- Evaluation / Execution --
----------------------------

-- | Evaluates an Expr value.
eval :: Scope -> Maybe Expr -> Maybe Int
eval _ Nothing = Nothing
eval _ (Just (Num x)) = Just x
eval s (Just (Var x)) = case lookup x s of
    Nothing -> error $ "Variable '" ++ x ++ "' not in scope"
    e       -> eval s e
eval s (Just (Add x y)) = do
    a <- eval s (Just x)
    b <- eval s (Just y)
    return $ a + b
eval s (Just (Sub x y)) = do
    a <- eval s (Just x)
    b <- eval s (Just y)
    return $ a - b
eval s (Just (Mul x y)) = do
    a <- eval s (Just x)
    b <- eval s (Just y)
    return $ a * b
eval s (Just (Div x y)) = do
    a <- eval s (Just x)
    b <- eval s (Just y)
    return $ a `div` b

-- | Gets the parsed expression portion from an expression parser.
getExpr :: Maybe (Expr, String) -> Maybe Expr
getExpr Nothing = Nothing
getExpr (Just (x, xs)) = Just x

-- | Executes a statement.
exec :: Scope -> Maybe Statement -> Scope
exec s Nothing = s
exec s (Just (Assignment (Var v) e)) = insert v e s

-- | Gets the parsed statement portion from a statement parser.
getStatement :: Maybe (Statement, String) -> Maybe Statement
getStatement Nothing = Nothing
getStatement (Just (x, xs)) = Just x

-- | Gets the expression portion from a statement.
getStatementExpr :: Maybe Statement -> Maybe Expr
getStatementExpr Nothing = Nothing
getStatementExpr (Just (Assignment v e)) = Just e

----------
-- Main --
----------

main = do
    contents <- readFile "test.f"
    let strings    = lines contents
        statements = map (getStatement . statement) strings
        scope      = foldl (\a s -> exec a s) (fromList []) statements
        lastExpr   = getStatementExpr (last statements)
        final      = eval scope lastExpr
    putStrLn $ show final
