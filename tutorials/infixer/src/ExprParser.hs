{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module ExprParser
        ( exprParser ) where

import AST
import Error
import Located
import Token

import GHC.Exts (IsList(..))
import Data.List (group, sort)
import Data.Set (Set, union)
import qualified Data.Set as Set (empty, singleton)

import Text.Megaparsec hiding (satisfy)
import Text.Megaparsec.Expr
import Text.Megaparsec.Pos
import Text.Megaparsec.Error (ShowToken)

type LocTok = Located FxrToken
type LocErr = Located Error

type OpParser = Parsec Dec [LocTok]
type ExprParser = OpParser Expr

type ErrTok = ErrorItem LocTok
type MegaErr = ParseError LocTok Dec

instance Stream [LocTok] where
        type Token [LocTok] = LocTok
        uncons []     = Nothing
        uncons (t:ts) = Just (t,ts)

        updatePos _ _ sPos (Loc (Span sL sC eL eC) _) = (pos sL sC, pos eL eC)
            where
                pos :: Int -> Int -> SourcePos
                pos l c = SourcePos (sourceName sPos) (intPos l) (intPos c)
                intPos :: Int -> Pos
                intPos = unsafePos . fromIntegral

exprParser :: [Assoc] -> [LocTok] -> Either LocErr Expr
exprParser asscs = handleEither . runParser (expr <* eof) "<stdin>"
    where
        table :: [[Operator OpParser Expr]]
        table = makeTable asscs
        expr :: ExprParser
        expr = makeExprParser term table
        term :: ExprParser
        term = parens expr <|> number <?> "term"
            where
                number :: ExprParser
                number = satisfy guard (Set.empty, Set.singleton (Label (fromList "number")))
                    where
                        guard :: LocTok -> Maybe Expr
                        guard (Loc p x) = case x of
                                TkNumber n -> Just (Num n p)
                                _          -> Nothing
                parens :: ExprParser -> ExprParser
                parens act = do
                        expect TkLParen
                        r <- act
                        expect TkRParen
                        return r

handleEither :: Either MegaErr Expr -> Either LocErr Expr
handleEither (Right expr) = Right expr
handleEither (Left perr)  = Left (Loc posSpan (ParsecError (toStrs errorUnexpected) (toStrs errorExpected)))
    where
        toStrs :: (MegaErr -> (Set ErrTok)) -> [String]
        toStrs get = map show' (toList (get perr))
            where
                show' :: ErrTok -> String
                show' err = case err of
                        Tokens ts -> concatMap (show . loc_in) ts
                        Label str -> toList str
                        EndOfInput -> "end of input"

        posSpan :: SrcSpan
        posSpan = Span lin col lin col
            where
                p :: SourcePos
                p = head (toList (errorPos perr))
                lin :: Int
                lin = (fromInteger . toInteger) (unPos (sourceLine p))
                col :: Int
                col = (fromInteger . toInteger) (unPos (sourceColumn p))

makeTable :: [Assoc] -> [[Operator OpParser Expr]]
makeTable = map (map toMegaOp) . groupByPrec
    where
        groupByPrec :: [Assoc] -> [[Assoc]]
        groupByPrec = reverse . group . sort

        toMegaOp :: Assoc -> Operator OpParser Expr
        toMegaOp assc = case assc of
                NonAssoc   name _ -> InfixN (bin name)
                LeftAssoc  name _ -> InfixL (bin name)
                RightAssoc name _ -> InfixR (bin name)
            where
                bin :: String -> OpParser (Expr -> Expr -> Expr)
                bin name = do
                        l <- expect (TkOp name)
                        return (Binary name l)

satisfy :: forall a . (LocTok -> Maybe a) -> (Set ErrTok, Set ErrTok) -> OpParser a
satisfy guard (unexp, expec) = token testExpr Nothing
    where
        testExpr :: LocTok -> Either (Set (ErrorItem LocTok), Set (ErrorItem LocTok), Set Dec) a
        testExpr lt = case guard lt of
                Just x  -> Right x
                Nothing -> Left (union unexp (errTkSingleton lt), expec, Set.empty)

expect :: FxrToken -> OpParser SrcSpan
expect t = satisfy guard (Set.empty, errTkSingleton (Loc NoSpanInfo t))
    where
        guard :: LocTok -> Maybe SrcSpan
        guard (Loc p x) = if t == x then Just p else Nothing

errTkSingleton :: LocTok -> Set ErrTok
errTkSingleton t = Set.singleton (Tokens (fromList [t]))
