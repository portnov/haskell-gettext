
module Data.Gettext.Plural where

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

data Expr =
    N
  | Literal Int
  | If Expr Expr Expr
  | Negate Expr
  | Not Expr
  | Binary BinOp Expr Expr
  deriving (Eq, Show)

order :: (Int -> Int -> Bool) -> (Int -> Int -> Int)
order op x y = if op x y then 1 else 0

logic :: (Bool -> Bool -> Bool) -> (Int -> Int -> Int)
logic op x y = if op (x /= 0) (y /= 0) then 1 else 0

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

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

eval :: Expr -> Int -> Int
eval N n = n
eval (Literal x) _ = x
eval (If cond true false) n =
  if eval cond n /= 0
    then eval true n
    else eval false n
eval (Binary op x y) n =
  evalOp op (eval x n) (eval y n)
eval (Negate x) n = negate $ eval x n

