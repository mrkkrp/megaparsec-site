---
title: First steps with Megaparsec
subtitle: Start your adventure in parsing here!
published: February 3, 2017
difficulty: 1
---

Are you looking for a way to start with parsing in Haskell? Then you have
arrived at the right place. This tutorial will introduce you to Megaparsec —
an advanced parsing library written in Haskell. While Megaparsec is capable
of a lot, it's also easy to use and easy to start with. In this tutorial we
will learn by writing a parser
for [URIs](https://en.wikipedia.org/wiki/Uniform_Resource_Identifier).

## The URI syntax

Let's take a look at the URI syntax:

```
scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]
```

We should remember that things in square brackets `[]` is optional, it may
or may not appear in a valid URI. `[]` may be even nested to express
possibility inside another possibility. We will handle all of this.

## Parsing the scheme

Let's include some modules:

```haskell
module Main (main) where

import Text.Megaparsec
import Text.Megaparsec.String
```

We will start with `scheme`. We will accept only schemes that are known to
us, such as:

* `data`
* `file`
* `ftp`
* `http`
* `https`
* `irc`
* `mailto`

To match a fixed sequence of characters, `string` is the best tool. To
express a choice, we use theh `(<|>)` operator:

```haskell
pScheme :: Parser String
pScheme = string "data" <|> string "file"
```

We could go on:

```haskell
pScheme :: Parser String
pScheme = string "data" <|> string "file" <|> string "ftp" <|> …
```

But it's not nice to be so repetitive. The `choice` combinator allows to
have as many alternatives as we put into its list:

```haskell
pScheme :: Parser String
pScheme = choice
  [ string "data"
  , string "file"
  , string "ftp"
  , string "http"
  , string "https"
  , string "irc"
  , string "mailto" ]
```

We already have something to play with. How to run it? For now, `parseTest`
will suffice. Load the code we have so far into the GHCi and execute:

```haskell
λ> parseTest pScheme ""
1:1:
unexpected end of input
expecting "data", "file", "ftp", "http", "https", "irc", or "mailto"
λ> parseTest pScheme "dat"
1:1:
unexpected "dat" or 'd'
expecting "data", "file", "ftp", "http", "https", "irc", or "mailto"
λ> parseTest pScheme "file"
"file"
λ> parseTest pScheme "irc"
"irc"
```

We get nice error messages for free.

After scheme, there should be a colon `:`, to require something to go after
something else, we use monadic bind or `do`-notation:

```haskell
data Uri = Uri
  { uriScheme :: String
  } deriving (Eq, Show)

pUri :: Parser Uri
pUri = do
  r <- pScheme
  char ':'
  return (Uri r)
```

If you don't understand what is going on with all these nasty monads, then
sure, just go on with the tutorial ignoring the magic, maybe it'll click
someday.

If we try to run `pUri`, we'll see that it requires `:` to follow the scheme
name now:

```haskell
λ> parseTest pUri "irc"
1:4:
unexpected end of input
expecting ':'
λ> parseTest pUri "irc:"
Uri {uriScheme = "irc"}
```

We are not done with the scheme parsing though. A good Haskell programmer
tries to define types in such a way so incorrect combinators/data are
impossible. Not any `String` is a valid scheme. Let's define a data type to
represent schemes and make our `pScheme` parser return value of that type:

```haskell
data Scheme
  = SchemeData
  | SchemeFile
  | SchemeFtp
  | SchemeHttp
  | SchemeHttps
  | SchemeIrc
  | SchemeMailto

```

## The URI type

## How to run Megaparsec parsers

## Alternatives

## `many` and `some`

## Conclusion

The library also has the best documentation among parsing libraries in
Haskell. You will find that looking
at [the Haddocks](https://hackage.haskell.org/package/megaparsec) is often
enough to find out how to do certain things.
