{
{-# OPTIONS_GHC -w #-}
module Lexer
        ( scanTokens ) where

import Token
import Located
}

%wrapper "posn"

-- whitespace
$nl           = [\n \r \f]
$whitechar    = [$nl \v \ ]
$white_no_nl  = $whitechar # \n                        -- all whites but \n

-- digits
$digit = 0-9

-- symbols
$symbol = [ \- \! \@ \# \$ \% \& \* \+ \/ \\ \<
            \= \> \^ \| \~ \? \` \[ \] \: \; \. ]

-- integers, one or more digits
@int = $digit+

-- operators, one or more symbols
@op = $symbol+

calc :-
        -- ignore whitespace and comments, which start with `#`
        $white_no_nl+   ;
        \#.*            ;

        -- new lines separate expressions
        $nl+            { ignore TkNewLine }

        -- numbers
        @int            { readTk (TkNumber . read) }
        -- operators
        @op             { readTk TkOp }
        -- parenthesis
        \(              { ignore TkLParen }
        \)              { ignore TkRParen }

        -- fixity
        infixN          { ignore TkInfixN }
        infixL          { ignore TkInfixL }
        infixR          { ignore TkInfixR }

        -- errors
        .               { readTk (TkError . head) }

{

ignore :: FxrToken -> AlexPosn -> String -> Located FxrToken
ignore tk = readTk (const tk)

readTk :: (String -> FxrToken) -> AlexPosn -> String -> Located FxrToken
readTk tkf p str = Loc spn (tkf str)
    where
        spn :: SrcSpan
        spn = posnToSpan p (length str)

        posnToSpan :: AlexPosn -> Int -> SrcSpan
        posnToSpan (AlexPn _ l c) w = Span l c l (c + w)

scanTokens :: String -> [Located FxrToken]
scanTokens = alexScanTokens
}
