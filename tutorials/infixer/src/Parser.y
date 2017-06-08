{
module Parser
        ( opsParser ) where

import AST
import Error
import Located
import Token
import Lexer
}

-- type of stream of tokens
%tokentype { Located FxrToken }

-- monad to run in the parser
%monad { Either (Located Error) }

-- error function
%error { parseError }

-- exported parsers
%name opsParser Stmts

-- how each token will be written in the grammar and what it stands for
-- the double dollars indicate to only get that part of the token
%token
        int               { Loc _ (TkNumber _) }
        op                { Loc _ (TkOp     _) }
        -- "("               { Loc $$ (TkLParen)  }
        -- ")"               { Loc $$ (TkRParen)  }
        "inN"             { Loc $$ (TkInfixN)  }
        "inL"             { Loc $$ (TkInfixL)  }
        "inR"             { Loc $$ (TkInfixR)  }
        ";"               { Loc $$ (TkNewLine) }

-- the grammar starts here
%%

-- generic rules

opt(p)
        : {- empty -}                   { Nothing }
        | p                             { Just $1 }

-- Happy works better with left recursion
some(p)
        : p                             { [$1] }
        | some(p) p                     { $1 ++ [$2] }

-- Happy works better with left recursion
sep1(p, s)
        : p                             { [$1] }
        | sep1(p, s) s p                { $1 ++ [$3] }

----------------------------------------

Stmts :: { [Assoc] }
        : sep1(Op, ";") opt(";")   { concat $1 }
        -- errors
        | {- empty -}                   {% Left (Loc (Span 0 0 0 0) NoOp) }

Op :: { [Assoc] }
        : "inN" some(op) int            { buildAssocs NonAssoc
                                                (map loc_in $2) (loc_in $3) }
        | "inL" some(op) int            { buildAssocs LeftAssoc
                                                (map loc_in $2) (loc_in $3) }
        | "inR" some(op) int            { buildAssocs RightAssoc
                                                (map loc_in $2) (loc_in $3) }
        -- errors
        | "inN" op                      {% Left (Loc $1 OpDecl) }
        | "inL" op                      {% Left (Loc $1 OpDecl) }
        | "inR" op                      {% Left (Loc $1 OpDecl) }

{
buildAssocs :: (String -> Precedence -> Assoc) -> [FxrToken] -> FxrToken -> [Assoc]
buildAssocs cf ops p = map (flip cf (tkNum p) . tkOp) ops

parseError :: [Located FxrToken] -> Either (Located Error) a
parseError (Loc spn (TkError c) : _) = Left (Loc spn (LexError c))
parseError (Loc spn _ : _)           = Left (Loc spn ParseError)
parseError []                        = Left (Loc (Span 0 0 0 0) ParseError)
}
