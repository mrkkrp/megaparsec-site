---
title: Developer tips
subtitle: 
published: 
code: 
difficulty: 4
---

# Expected tokens #
When you get a [`ParserError`](https://www.stackage.org/haddock/lts-7.18/megaparsec-5.0.1/Text-Megaparsec.html#t:ParseError), you can use the `errorExpected` function together with the [`toList`](https://www.stackage.org/haddock/lts-7.18/containers-0.5.7.1/Data-Set.html#v:toList) to get a list of expected [`ErrorItem`'s](https://www.stackage.org/haddock/lts-7.18/megaparsec-5.0.1/Text-Megaparsec.html#t:ErrorItem). These `ErrorItem`'s can be one of the following:

* `Tokens`, in case there is a single token that was expected (such as a single character).
* `Label`, in case a parser with a certain label comes next which expects various tokens.
* `EndOfInput`, in case there are no more parsers.

Expected here does not mean that, for example, the indicated token `"A"` MUST be matched. This is only true when there is a single item in the expected list. In case of multiple items only one of them has to be matched.

# Developing your parser #

## Top-down approach (don't do it) ##
When you are re-using an existing grammar (or even when developing your own) you might be inclined to read the grammar top-down and also start developing your parser in a top-down fashion. There are various pitfalls when you develop your parser top-down. These pitfalls car occur when you want to test parts of your parser already even when it's not complete.

### Non-terminating parsers ###
When you have a partially implemented grammar rule the parser might not yet consume input. This is the case when your rule is like `A = B*` where `*` denotes zero-or-more. When writing a parser for this rule it will always succeed when parser B does not match anything. Consider `B = "Token here"` for input `"Doesn't start with Token"`. The parser for B will then fail immediately because `D` does not match `T` (the first letter), on which parser A does then not consume any input but always succeed. This is not a problem just yet, but when you then use parser A by another parser (`Z`) that tries to call parser A multiple times your code starts to loop indefinitely. For example `Z = A*` or `Z = A+`, where `+` denotes one-or-more. The type of this parser is then [`bottom`](https://wiki.haskell.org/Bottom) because it does not terminate. Technically it's not always true that parser B does not consume any input, but very likely you are using megaparsec's [`string`](https://www.stackage.org/haddock/lts-7.18/megaparsec-5.0.1/Text-Megaparsec-Char.html#v:string) or [`string'`](https://www.stackage.org/haddock/lts-7.18/megaparsec-5.0.1/Text-Megaparsec-Char.html#v:string-39-) function which automatically backtracks.

### Typeclasses ###
When you are implementing your parser functions as part of a typeclass you will need to help GHC making sense of the types and indicating which instances need to be available for another instance to work. Consider these rules and code (pseudo-code does not compile):

```
A = B C
B = "hello "
C = A* "world"
```

With these types:
```haskell
type Hello = String
type World = String
newtype RuleA = R_A RuleB RuleC
newtype RuleB = R_B Hello
newtype RuleC = R_C RuleA World
```

Input "hello hello world world" could be parsed with this sequence:
```
A ->
  B -> "hello "
  C ->
    A ->
      B -> "hello "
      C -> "world"
    C -> "world"
```
The actual parsing is irrelevant to the typeclass problem and is only added to give a complete example.

Now when you start writing your typeclasses you might do something like

```haskell
instance (Rule Parser B, Rule Parser C) => Rule Parser A where
instance Rule Parser B where
instance Rule Parser A => Rule Parser C where
```

for a class like

```haskell
class Rule Parser a where
  parse :: Parser a
```

You will need the [GHC extension](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#undecidable-instances) `UndecidableInstances` for this to work as by default GHC is very restrictive.

Now what happens when you develop your parser top-down and decide to start to implement Rule C first and leave out Rule B for the moment, you get:

```haskell
instance Rule Parser C => Rule Parser A where
instance Rule Parser A => Rule Parser C where
```

Since you are using `UndecidableInstances` you loosen up GHC restrictions, but at the same time you are also disabling a check in GHC that can detect loops in these constraints. Now that the check for the loop is disabled, GHC will no longer terminate, and your program does not compile anymore!
