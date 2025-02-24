-- | This module contains definitions for plural form selection expressions AST,
-- and an evaluator function for such expressions.
--
module Data.Gettext.Plural
  (
   -- * Data types
   BinOp (..), Expr (..),
   -- * Expressions
   eval
  ) where

import Data.Bits (xor)

-- | Supported binary operations
data BinOp =
    Equals
  | NotEquals
  | Greater
  | NotGreater
  | Less
  | NotLess
  | And
  | Or
  | Xor
  | Mod
  | Plus
  | Minus
  | Multiply
  | Divide
  deriving (Eq, Show)

-- | Plural form selection expression AST
data Expr =
    N                        -- ^ The @n@ variable
  | Literal Int              -- ^ Literal number
  | If Expr Expr Expr        -- ^ Ternary operator (... ? ... : ...)
  | Negate Expr              -- ^ Unary arithmetic negation (as in @-1@).
  | Not Expr                 -- ^ Unary logic negation (as in @! (n == 1)@)
  | Binary BinOp Expr Expr   -- ^ Binary operation
  deriving (Eq, Show)

order :: (Int -> Int -> Bool) -> (Int -> Int -> Int)
order op x y = if op x y then 1 else 0

logic :: (Bool -> Bool -> Bool) -> (Int -> Int -> Int)
logic op x y = if op (x /= 0) (y /= 0) then 1 else 0

evalOp :: BinOp -> (Int -> Int -> Int)
evalOp Equals = order (==)
evalOp NotEquals = order (/=)
evalOp Greater = order (>)
evalOp NotGreater = order (<=)
evalOp Less = order (<)
evalOp NotLess = order (>=)
evalOp And = logic (&&)
evalOp Or = logic (||)
evalOp Xor = logic xor
evalOp Mod = mod
evalOp Plus = (+)
evalOp Minus = (-)
evalOp Multiply = (*)
evalOp Divide = \x y ->
  if y == 0
    then error "Division by zero in plural form selection expression"
    else x `div` y

-- | Evaluate the expression
eval :: Expr  -- ^ Expression
     -> Int   -- ^ Number
     -> Int   -- ^ Plural form index defined by expression
eval N n = n
eval (Literal x) _ = x
eval (If cond true false) n =
  if eval cond n /= 0
    then eval true n
    else eval false n
eval (Binary op x y) n =
  evalOp op (eval x n) (eval y n)
eval (Negate x) n = negate $ eval x n
eval (Not x) n = if eval x n == 0 then 1 else eval x n
