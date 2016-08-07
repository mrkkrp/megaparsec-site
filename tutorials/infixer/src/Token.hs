module Token where

data FxrToken
  -- literals
  = TkNumber            { tkNum :: Integer }

  -- operators
  | TkOp                { tkOp :: String }

  -- Parenthesis
  | TkLParen
  | TkRParen

  -- fixity
  | TkInfixN
  | TkInfixL
  | TkInfixR

  | TkNewLine
  | TkError             { tkErr :: Char }
  deriving Eq

instance Show FxrToken where
        show tk = case tk of
                TkNumber num -> show num
                TkOp     str -> str
                TkLParen     -> "("
                TkRParen     -> ")"
                TkInfixN     -> "infixN"
                TkInfixL     -> "infixL"
                TkInfixR     -> "infixR"
                TkNewLine    -> "<\\n>"
                TkError    c -> "ERROR: " ++ [c]
