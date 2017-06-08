module Error where

import Data.List (intercalate)

data Error
  = NoArgFile
  | NoFixity String
  | LexError Char
  | ParseError
  -- specific parse errors
  | NoOp
  | OpDecl
  -- Parsec errors
  | ParsecError [String] [String]

instance Show Error where
        show err = case err of
                NoArgFile         -> "usage: infixer FILE"
                NoFixity s        -> s ++ "has no fixity declaration"
                LexError c        -> "unexpected character: " ++ [c]
                ParseError        -> "parsing error"
                NoOp              -> "there should be at least one operator listed"
                OpDecl            -> "fixity must have operator and precedence"
                ParsecError us es -> "parsing error:\n " ++
                                        caseMsg "unexpected " us ++
                                        caseMsg "expected " es
                    where
                        caseMsg :: String -> [String] -> String
                        caseMsg msg strs = case strs of
                                [] -> ""
                                _  -> msg ++ intercalate ", " strs ++ "\n "
