---
title: Writing a fast parser
subtitle: Practical recommendations
published: September 11, 2016
---

If performance of your Megaparsec parser is worse that you hoped, there may
be ways to improve it. This short guide will instruct you what to attempt,
but you should always check if you're getting better results by profiling
your parsers (that's the only way to understand if you are doing the right
thing when tuning performance).

* If your parser uses monad stack instead of plain `Parsec` monad (which is
  a monad transformer over `Identity` too, but it's much more lightweight),
  make sure you use at least version 0.5 of `transformers` library, and at
  least version 5.0 of `megaparsec`. Both libraries have critical
  performance improvements in those versions, so you can just get better
  performance for free.

* `Parsec` monad will be always faster then `ParsecT`-based monad
  transformers. Avoid using `StateT`, `WriterT`, and other monad
  transformers unless absolutely necessary. When you have relatively simple
  monad stack, for example with `StateT` and nothing more, performance of
  Megaparsec parser will be on par with Parsec. The more you add to the
  stack, the slower it will be.

* The most expensive operation is backtracking (you enable it with `try` and
  it happens automatically with `tokens`-based parsers). Avoid building long
  chains of alternatives where every alternative can go deep into input
  before failing.

* Inline generously (when it makes sense, of course). You may not believe
  your eyes when you see how much of a difference inlining can do,
  especially for short functions.

The same parser can be written in many ways. Think about your grammar and
how parsing happens, when you get some experience with this process, it will
be much easier for you to see how to make your parser faster. Sometimes
however, making a parser faster will also make code less readable. If
performance of your parser is not a bottleneck in the system you are
building, consider preferring readability over performance.
