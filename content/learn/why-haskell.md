---
description: ""
title: "Why Haskell"
draft: false
weight: 200
bref:
toc: true
---

## The Usual FP stuff

- Abstractions specifically for Programming (`Functor`, `Monad`, `Applicative`, `Traversable`, etc...)
- More expressive type systems through concepts like parametricity (or "theorems for free")
- Higher Order functions as ways to write our own "Control Structures" (as opposed to requiring they all be language features, e.g. if-else clauses)
- Programs as Proofs (The Curry-Howard correspondence).

## Typeclasses

The aforementioned abstractions are pretty awkward in languages like `F#` which don't have typeclasses.

Typeclasses allow these concepts to be defined at a semantic level for the types, allowing for the types to be more expressive.

## Type Reified IO

When you read about the benefits of FP, the words "pure functions" often come up.

While most FP practitioners strive to separate pure functions (functions with no IO side effects that always produce the same output when given the same inputs) from impure functions (functions that have an effect on internal state like RAM or external systems like DBs or logging), its often entirely up to the discipline of the programmers involved.

For Haskell all functions are "pure", and some functions have an output value with a type like `IO a`. This is an IO "action" that performs some IO and also returns a value of type `a`. It is the act of unrolling this value that is impure. An analogy to help understand this concept is the difference between a recipe to cook spaghetti bolognese and the spaghetti bolognese that is produced by enacting the recipe. Everytime you look up the recipe in the cookbook, its value is the same, but when you run the recipe the results may vary due to all kinds of external circumstances. Similarly in Haskell a function that produces a value of `IO a` will always produce the same "recipe" given the same inputs, when that value is run (`IO a -> a`) the value of type `a` that is produced may be different from case to case. You, never actually "run" these recipes yourself, you combine all these values to produce a high level program like `main :: IO ()` that the runtime will run in one go.

Languages like Scala where running IO inside a construct like this is optional (and is not the default). It is the opposite in Haskell, by default you have to define `IO` operations inside the `IO` type, having to go out of your way to do "the less safe thing".

## Exception Free

(Or more convenient to be *synchronous* exception free)

Exceptions will always exist, but basically we should be cutting down the sources to make programming and error handling more managable. "Asynchronous" exceptions (exceptions like "out of memory" or "Another thread died") are the things we can't really do anything about. But exceptions like "index out of bounds" or "null pointer" are things we should be able to get rid of, and prevent using a combination of types and `Either`/`Maybe`.

i.e given a function you should be able to enforce, using types and exhaustive pattern matching, that it is not the source of a "programmer" instigated exception (for a lot of cases at least).

## "Industrial Strength" libraries available

- AWS
    - [`amazonka`](https://hackage.haskell.org/package/amazonka)
- RESTful web services
    - [`warp`](https://hackage.haskell.org/package/warp)
    - [`servant`](https://hackage.haskell.org/package/servant)
    - [`airship`](https://hackage.haskell.org/package/airship)
- NEO4J (+ BOLT)
    - [`hasbolt`](https://hackage.haskell.org/package/hasbolt)
- Kafka (Client)
    - [`hw-kafka-client`](https://github.com/haskell-works/hw-kafka-client)
    - [`hw-kafka-conduit`](https://github.com/haskell-works/hw-kafka-conduit)
    - [`pipes-kafka`](https://github.com/boothead/pipes-kafka)

For more, see [State of the Haskell Ecosystem](https://github.com/Gabriel439/post-rfc/blob/master/sotu.md)

## Infrastructure free package sharing

[`mafia`](https://github.com/ambiata/mafia) allows for sharing internal projects (and use external libraries) from github (similar to librarian etc...)

## Lazy evaluation by default

"Lazy evaluation" is a term used to often describe semantically different things.

Theres the "operational" semantic that it implies, i.e *how* a Haskell program is evaluated. This is technically
what "Lazy Evaluation" refers to.

Then there is the "denotational semantic", or *what* a Haskell program computes, which is properly referred to as
"Non-strict semantics".

In terms of performace and such concepts, it is the operational semantic that comes into play.

Generally Lazy evaluation is the best default because it is the most general. I.e if you have already eagerly evaluated a value, you cannot undo that. But with an unevaluated thunk, you can always opt to force it when it is beneficial to do so.

The more generally applicable your code is, the more re-usable or "modular" it can be. If you have a function that eagerly computes a value, you will have to rewrite a special instance of it if a case arises where the full value isn't needed and computing the full value becomes expensive. Everytime you see the word `break` or a `return` inside a for loop in your typical imperative language, you have probably seen something that could be written in a more
concise manner in a lazily-evaluated functional language.

In terms of performance, it will in most cases out perform eager evaluation as "only the part of the result that is needed will get computed", the caveat may be the timing of the evaluation, as it will happen "when it is needed" which sometimes is not the most appropriate time to compute something without introducing latency.

Another caveat is the memory leaks that can occur in building up a thunk that may never get forced. However both of these issues have effects that are easily profiled and traced, with simple solutions (for the former due to purity of functions, you might be able to evaluate the thunk earlier and in parallel, the latter, explicitly force the thunk).

On the denotational semantics side of things, there is the "infinite" (or "Coinductive") types and values that you can express.

## Profilable runtime

Memory profiling is pretty important with a Lazy-by-default language like Haskell.

CPU and memory profiling are provided as part of the haskell runtime
