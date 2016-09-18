---
title: Parsing a simple imperative language
subtitle: Based on original Parsec tutorial
published: May 14, 2015
---

This tutorial will present how to parse a subset of a simple imperative
programming language called *WHILE* (introduced in a book “Principles of
Program Analysis” by Nielson, Nielson and Hankin). It includes only a few
statements and basic boolean/arithmetic expressions, which makes it a nice
material for a tutorial.

1. [Imports](#imports)
2. [The language](#the-language)
3. [Data structures](#data-structures)
4. [Lexer](#lexer)
5. [Parser](#parser)
6. [Expressions](#expressions)
7. [Notes](#notes)

## Imports

First let's specify the name of the module:

```haskell
module ParseWhile where
```

And then import the necessary libraries:

```haskell
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L
```

## The language

The grammar for expressions is defined as follows:

```
a   ::= x | n | - a | a opa a
b   ::= true | false | not b | b opb b | a opr a
opa ::= + | - | * | /
opb ::= and | or
opr ::= > | <
```

Note that we have three groups of operators — arithmetic, boolean and
relational ones.

And now the definition of statements:

```
S ::= x := a | skip | S1; S2 | ( S ) | if b then S1 else S2 | while b do S
```

We probably want to parse that into some internal representation of the
language (abstract syntax tree). Therefore we need to define the data
structures for the expressions and statements.

## Data structures

We need to take care of boolean and arithmetic expressions and the
appropriate operators. First let's look at the boolean expressions:

```haskell
data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
             deriving (Show)
```

Binary boolean operators:

```haskell
data BBinOp = And | Or deriving (Show)
```
Relational operators:

```haskell
data RBinOp = Greater | Less deriving (Show)
```

Now we define the types for arithmetic expressions:

```haskell
data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
             deriving (Show)
```

And arithmetic operators:

```haskell
data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
              deriving (Show)
```

Finally let's take care of the statements:

```haskell
data Stmt = Seq [Stmt]
          | Assign String AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip
            deriving (Show)
```

## Lexer

Having all the data structures we can go on with writing the code to do
actual parsing. Here we will define *lexemes* of our language. When writing
lexer for a language it's always important to define what counts as
whitespace and how it should be consumed. `space` from
`Text.Megaparsec.Lexer` module can be helpful here:

```haskell
sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"
```

`sc` stands for “space consumer”. `space` takes three arguments: parser that
parses single whitespace character, parser for line comments, and parser for
block (multi-line) comments. `skipLineComment` and `skipBlockComment` help
quickly create parsers to consume the comments. (If our language didn't have
block comments, we could pass `empty` as third argument of `space`.)

Next, we will use strategy where whitespace will be consumed *after* every
lexeme automatically, but no before it. Let's define a wrapper to achieve
this:

```haskell
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
```

Perfect. Now we can wrap any parser in `lexeme` and it will consume trailing
whitespace with `sc`. Since we often want to parse some “fixed” string,
let's define one more parser called `symbol`. This will take string as
argument and parse this string and whitespace after it.

```haskell
symbol :: String -> Parser String
symbol = L.symbol sc
```

With these tools we can create other useful parsers:

```haskell
-- | 'parens' parses something between parenthesis.

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | 'integer' parses an integer.

integer :: Parser Integer
integer = lexeme L.integer

-- | 'semi' parses a semicolon.

semi :: Parser String
semi = symbol ";"
```

Great. To parse various operators we can just use `symbol`, but reserved
words and identifiers are a bit trickier. There are two points to note:

* Parsers of reserved words should check that the parsed reserved word is
  not a prefix of an identifier.

* Parsers of identifiers should check that parsed identifier is not a
  reserved word.

Let's express it in code:

```haskell
rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

rws :: [String] -- list of reserved words
rws = ["if","then","else","while","do","skip","true","false","not","and","or"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x
```

`identifier` may seem complex, but it's really simple. We just parse
sequence of characters where first character is a letter and the rest is
several characters where every one of them can be either letter or
number. Once we have parsed such string, we check if it's in list of
reserved words, fail with informative message if it is, and return the
result otherwise.

Note the use of `try` in `identifier`. This is necessary to backtrack to
beginning of the identifier in cases when `fail` is executed. Otherwise
things like `many identifier` would fail on such identifiers instead of just
stopping.

And that's it, we have just written lexer for our language, now we can start
writing parser.

## Parser

As already mentioned a program in this language is simply a statement, so
the main parser should basically only parse a statement. But remember to
take care of initial whitespace — our parsers only get rid of whitespace
after the tokens!

```haskell
whileParser :: Parser Stmt
whileParser = sc *> stmt <* eof
```

Now because any statement might be actually a sequence of statements
separated by semicolon, we use `sepBy1` to parse at least one statement. The
result is a list of statements. We also allow grouping statements by the
parenthesis, which is useful, for instance, in the `while` loop.

```haskell
stmt :: Parser Stmt
stmt = parens stmt <|> stmtSeq

stmtSeq :: Parser Stmt
stmtSeq = f <$> sepBy1 stmt' semi
  -- if there's only one stmt return it without using ‘Seq’
  where f l = if length l == 1 then head l else Seq l
```

Now a single statement is quite simple, it's either an `if` conditional, a
`while` loop, an assignment or simply a `skip` statement. We use `<|>` to
express choice. So `a <|> b` will first try parser `a` and if it fails (but
without actually consuming any input) then parser `b` will be used. *Note:
this means that the order is important.*

```haskell
stmt' :: Parser Stmt
stmt' = ifStmt <|> whileStmt <|> skipStmt <|> assignStmt
```

If you have a parser that might fail after consuming some input, and you
still want to try the next parser, you should look into `try`
combinator. For instance `try p <|> q` will try parsing with `p` and if it
fails, even after consuming the input, the `q` parser will be used as if
nothing has been consumed by `p`.

Now let's define the parsers for all the possible statements. This is quite
straightforward as we just use the parsers from the lexer and then use all
the necessary information to create appropriate data structures.

```haskell
ifStmt :: Parser Stmt
ifStmt =
  do rword "if"
     cond  <- bExpr
     rword "then"
     stmt1 <- stmt
     rword "else"
     stmt2 <- stmt
     return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt =
  do rword "while"
     cond <- bExpr
     rword "do"
     stmt1 <- stmt
     return $ While cond stmt1

assignStmt :: Parser Stmt
assignStmt =
  do var  <- identifier
     void $ symbol ":="
     expr <- aExpr
     return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = Skip <$ rword "skip"
```

## Expressions

What's left is to parse the expressions. Fortunately Megaparsec provides a
very easy way to do that. Let's define the arithmetic and boolean
expressions:

```haskell
aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators
```

Now we have to define the lists with operator precedence, associativity and
what constructors to use in each case.

```haskell
aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [Prefix (symbol "-" *> pure Neg) ]
  , [ InfixL (symbol "*" *> pure (ABinary Multiply))
    , InfixL (symbol "/" *> pure (ABinary Divide)) ]
  , [ InfixL (symbol "+" *> pure (ABinary Add))
    , InfixL (symbol "-" *> pure (ABinary Subtract)) ]
  ]

bOperators :: [[Operator Parser BExpr]]
bOperators =
  [ [Prefix (rword "not" *> pure Not) ]
  , [InfixL (rword "and" *> pure (BBinary And))
    , InfixL (rword "or" *> pure (BBinary Or)) ]
  ]
```

In case of prefix operators it is enough to specify which one should be
parsed and what is the associated data constructor. Infix operators are
defined similarly, but there are several variants of infix constructors for
various associativity. Note that the operator precedence depends only on the
order of the elements in the list.

Finally we have to define the terms. In case of arithmetic expressions, it
is quite simple:

```haskell
aTerm :: Parser AExpr
aTerm = parens aExpr
     <|> Var      <$> identifier
     <|> IntConst <$> integer
```

However, the term in a boolean expression is a bit more tricky. In this
case, a term can also be an expression with relational operator consisting
of arithmetic expressions.

```haskell
bTerm :: Parser BExpr
bTerm =  parens bExpr
     <|> (rword "true"  *> pure (BoolConst True))
     <|> (rword "false" *> pure (BoolConst False))
     <|> rExpr
```

Therefore we have to define a parser for relational expressions:

```haskell
rExpr :: Parser BExpr
rExpr =
  do a1 <- aExpr
     op <- relation
     a2 <- aExpr
     return $ RBinary op a1 a2

relation :: Parser RBinOp
relation =  (symbol ">" *> pure Greater)
        <|> (symbol "<" *> pure Less)
```

And that's it. We have a quite simple parser able to parse a few statements
and arithmetic/boolean expressions.

## Notes

If you want to experiment with the parser inside GHCi, these functions might
be handy:

* `parseTest p input` applies parser `p` on input `input` and prints
  results.

* `parseFromFile p filename` applies parser `p` on contents of file
  `filename`.

----

Original Parsec tutorial in Haskell Wiki:

[https://wiki.haskell.org/Parsing_a_simple_imperative_language](https://wiki.haskell.org/Parsing_a_simple_imperative_language)
