---
title: How to introduce custom error messages
subtitle: With Megaparsec 5 it's possible to
published: August  7, 2016
---

One of the advantages of Megaparsec 5 is the ability to use your own data
types as part of data that is returned on parse failure. This opens up the
possibility to tailor error messages to your domain of interest in a way
that is quite unique to this library. Needless to say, all data that
constitutes a error message is typed in Megaparsec 5, so it's easy to
inspect it and manipulate after the fact.

## The goal

In this tutorial we will walk through creation of parser of an existing
library called
[`cassava-megaparsec`](https://hackage.haskell.org/package/cassava-megaparsec),
which is an alternative parser for the popular `cassava` library that allows
to parse CSV data. The default parser features not very user-friendly error
messages, so I was asked to design a better one using Megaparsec.

In addition to standard error messages, the library can report problems that
have to do with using methods to `FromRecord` and `FromNamedRecord` type
classes that describe how to transform collection of `ByteString`s into a
particular instance of those type classes. While performing the conversion,
things may go wrong, and we would like to use a special data constructor in
these cases.

The complete source code can be found in
[this GitHub repository](https://github.com/stackbuilders/cassava-megaparsec).

## Language extensions and imports

We will need some language extensions and imports, here is the top of
`Data.Csv.Parser.Megaparsec` almost literally:

```haskell
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module Data.Csv.Parser.Megaparsec
  ( Cec (..)
  , decode
  , decodeWith
  , decodeByName
  , decodeByNameWith )
where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Char (chr)
import Data.Csv hiding
  ( Parser
  , record
  , namedRecord
  , header
  , toNamedRecord
  , decode
  , decodeWith
  , decodeByName
  , decodeByNameWith )
import Data.Data
import Data.Vector (Vector)
import Data.Word (Word8)
import Text.Megaparsec
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Csv              as C
import qualified Data.HashMap.Strict   as H
import qualified Data.Set              as S
import qualified Data.Vector           as V
```

Note that there are two imports for `Data.Csv`, one for some common things
like names of type class that I want to keep unprefixed and the second one
for the rest of the stuff (qualified as `C`).

## What is `ParseError` actually?

To start with custom error messages we should take a look at how parse
errors are represented in Megaparsec 5.

…

## Defining custom error component

…

## Top level API and helpers

…

## The parser

…

## Conclusion

I hope this walk-through has demonstrated that it's quite trivial to insert
your own data into Megaparsec error messages. This way it's also possible to
pump out some data from failing parser or just keep track of things in a
type-safe way, which is one thing we should always care about when writing
Haskell programs.
