module AST where

import Located

data Expr
  = Binary String  SrcSpan Expr Expr
  | Num    Integer SrcSpan

exprSpan :: Expr -> SrcSpan
exprSpan expr = case expr of
        Binary _ p _ _ -> p
        Num    _ p     -> p


instance Show Expr where
        show expr = case expr of
                Binary op _ l r -> unwords ["(", show l, op, show r, ")"]
                Num    x _      -> show x

type Precedence = Integer
data Assoc
  = NonAssoc   { opName :: String, opPrec :: Precedence }
  | LeftAssoc  { opName :: String, opPrec :: Precedence }
  | RightAssoc { opName :: String, opPrec :: Precedence }

instance Show Assoc where
        show ass = case ass of
                NonAssoc   op pr -> "infixN " ++ op ++ " " ++ show pr
                LeftAssoc  op pr -> "infixL " ++ op ++ " " ++ show pr
                RightAssoc op pr -> "infixR " ++ op ++ " " ++ show pr

instance Eq Assoc where
        l == r = opName l == opName r

instance Ord Assoc where
        compare l r = compare (opPrec l) (opPrec r)
