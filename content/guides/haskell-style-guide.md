---
description: ""
title: "Haskell Style Guide"
draft: false
weight: 200
bref:
toc: true
---

## Build Settings

- prepend project names for internally developed libraries with `companyname-` in the cabal file
- cabal filename: `projname.cabal`
- `== x.y.*` constraints where possible
    - in practice they sometimes get widened to `>= a.b.c && < x.y` bounds, but it would be good to start from `x.y.*`.
- `LANGUAGE` pragmas per file please, no `default-extensions` in the cabal file.
    - This way, we are *less* dependent on `cabal` for things.
        - for e.g. we can load individual modules directly with `ghci -package-db=<...>` rather than `cabal repl`, which will run a build before loading up the repl.
- All main/production code in `/src`
- All test source in `/test`
- All main modules for project binaries in `/main`
- Use [`haskell-mafia/mafia`](https://github.com/haskell-mafia/mafia) to build projects instead of `stack` (`mafia` doesn't require additional config
- Sandboxes always (*sigh*)
- git submodules to manage fetching and versioning of dependencies that we can't or don't want to use from hackage (`mafia` manages this)

## Libraries

- `{-# LANGUAGE NoImplicitPrelude #-}` always
- `Preamble` over `Prelude` always

## Strings

- `String` never, `String` is a type synonym for `[Char]`, its a good model but has terrible performance.
    - That and it ignores the entire issue of text encoding that is part of the Text semantic.
- `Ultra.Data.Text` is the "default choice" (it is at the time of this writing just a re-export of the strict `Data.Text`)

## Imports

- Always use explicit import lists or qualified imports
    - Makes the code more stable with changes to the imported libraries
    - Makes it easier for someone to read the code to figure out where all the functions used within come from.
        - In theory this makes it easier for someone to develop on the codebase with a minimally featured text editor.
        - Not that anyone has to, but sometimes you might be deving on an EC2 box or something which doesnt have your personal dev environment setup.
        - Just nice to support that scenario because its not hard to do (at least with Haskell it isnt).
    - Exception: `Preamble` *can and should always* be imported unqualified (without an import list)
    - Exception 2: Modules local to the project *can* be imported unqualified (without an import list)
- Separate "groups" by a blank line (See the below example for a description of the groups)
- Order the import list as follows:
    - Typeclasses (if any) first
    - Types (if any)
    - operators (if any)
    - functions/values (if any)
    - within each category, order them alphabetically.
- Do **not** indent simple imports (i.e non-qualified imports) so that the Module names line up (do **not**)
    - (**do not do it**)

Example:

``` haskell
-- Imagine this project is called "Boggle", so that all the projects belonging to this project
-- are of the form `Boggle.*`

-- The first group is imports that are from modules local to the project.
import Company.Boggle.Core (BoggleClass(..), BoggleType(..), (<+>), boggle)
import qualified Company.Boggle.Foo as F

-- afterwards comes imports for any other internally developed/maintained libraries/forks
import Company.Nile.Core (someFunctionFromNiles) -- internally developed project
import Company.Nile.Sub (someOTherNilesFunc)

-- group them together by the "top-level" module, skipping over "Company." if it exists...
import Company.Other.Core (otherFunc)
import Company.Other.Library (func)

import qualified Ultra.Data.Text as T -- internally maintained fork

-- then a single group of alphabetically sorted "base/external/third-party" modules.
import qualified Data.ByteString as BS
import Data.List (replicate)
import System.IO (putStrLn)

-- finally include Preamble
import Preamble
```

## Exports

- Production modules should have an explicit export list, sorted in similar fashion to the import lists:
    - Typeclasses first
    - Types
    - Operators
    - Functions
    - Other domain specific concepts, usually "values" of some type (like `Parsers`)

- For documentation purposes use headers like `Typeclasses`, `Types`, `Operators`, `Functions` to separate the groups.

- Test modules defining properties should not have export lists (just export everything so tests can be run directly from the REPL).
    - Test modules containing things like helpers (e.g. `Gen`s and `Arbitrary` instances) can still have export lists

- Export lists should export everything that is "safe", i.e everything that doesnt violate a contraint that is supposed to be protected by a type.
    - Do not hide something to make the API "neater", layer the code so that your internals arent in the API modules.
    - This makes things more testable.

- Indentation as in example below

example:

``` haskell
module Thrombosis (
  -- * Typeclasses
    Class(..)
  -- * Types
  , Type(..)
  -- * Functions
  , func
  -- * Values
  , zero
  -- ** Can have subheaders
  , somethingRelatedToSubHeader
  ) where
  -- note the indentation
```

## Laziness

- Strict datatypes and lazy functions please.
    - Strict types (either using `{-# LANGUAGE StrictData #-}` or using bang patterns)
    - Strict data will prevent most of the memory leaks you would otherwise come across

- Function arguments should be left lazy by default unless theres a good specific reason to do otherwise.
    - Eager evaluation destroys [function reuse](http://augustss.blogspot.com.br/2011/05/more-points-for-lazy-evaluation-in.html)

- Sometimes you do indeed need a strict evaluation for a particular function argument. The most notable case is
  an accumulator in a `fold` or a counter in a `StateT` computation where you keep modifying it inside, but only do something with the value at the end.
    - In these cases, you are continually building thunks around these values, only evaluating them all the way at the end.
      This will naturally lead to a memory leak and its important to make sure that its evaluated at each step.

- Be very aware that bang patterns only cause the argument to be evaluated to weak head normal form (WHNF).
  You can google the term for a specific definition. But basically the value thunk is only going to be evaluated and reduced enough to figure out which pattern/case it would match.
  In the case of types like `Int` and etc... this amounts to a full strict evaluation. But if you have a thunk for a value of type list (`:: [a]` for some `a`) then it will "evaluate" it until
  it is determined that the value is of the form `_ : _` or `[]`, where `_` indicates that it is left as an unevaluated thunk,
  unless its evaluation was somehow necessary to determine which data constructor is used to construct the value (`:` or `[]` in this case).

Strict Data types with bang patterns (also keep in mind the above caveat):

``` haskell
data Haskell =
    SomeInt !Int
  | IntAndText !Int !T.Text
  | SomeMaybeText !(Maybe T.Text)
    deriving (Show, Eq)
```

## Pragmas

- one pragma to a line

example:

``` haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
```

## Extensions

Extensions are basically an explorative feature of haskell

It allows for a new feature to be provided in an opt-in fashion so as not to step on anyones toes while not having to outright deny the addition of new features without giving them a try.

As such, the safety of extensions varies, there are some that were unsafe, but have been honed into something safe and useful over time.

There are some which are completely safe, and really just boil down to allowing for peoples different preferences (e.g. `NoImplicitPrelude`).

There are some that weaken some of the safety checks of things like the type class system when they reject valid code because they cannot verify certain properties

There are some that are just fundamentally unsafe.

- standard extensions (i.e these are definitely OK):
    - `NoImplicitPrelude`
    - `LambdaCase`
    - `OverloadedStrings`
    - `GeneralizedNewtypeDeriving`
    - `ScopedTypeVariables`
    - `RankNTypes`

- sometimes useful/necessary and totally OK, if you need them dont hesitate to add them:
    - `GADTs`
    - `GADTSyntax`
    - `TemplateHaskell`

- use with extreme caution
    - `RecordWildCards`
        - This may seem very convenient, but the main reason against them is that they arent future proof.
        - It is akin to using a catch all pattern match (i.e `_ -> blah`)
        - If you really want something like this, consider using `NamedFieldPuns` instead
    - There are so many that need to go here... will add them as we go.

## Show and Read instances

- No pretty printing please, just use `deriving` or define an instance as good as.
    - If you dont know what "good as" is saying, just keep this in mind:
        - QuickCheck will display the generated and shrunken values that cause tests to fail.
        - It uses the `Show` instance to do that
        - We want to be able to cut and paste that text straight into a REPL to run the relevent production code
          or the property test itself.
        - Hence "good as" means that you can cut and paste that text into a REPL and get the value in the repl
            - e.g. you might write a smart constructor for the type, your `Show` instance can render values
              as how they would look if you were to create them with the smart constructor (as long as it contains all the important data that distinguishes values of that type)
            - e.g. the way the `Show` instance for `Map` renders maps as how they would be constructed if you used `fromList` to construct them.


## General style

- 2 space indenting
- no record selectors on sum types (these result in unsafe partial functions)

## Future Proofing

This basically amounts to avoiding (where we can, and this section basically outlines some areas where we can) situations
where a library we depend on changes its interface in some fashion, and our code silently compiles fine but is now broken.

- Don't use catch all patterns.
  - Patterns like `Just _` are fine, its stuff like `foo _ = bar` or ` _ -> bar` that are not OK.
  - If a new case is added, these functions need to be rethought to see if a non-trivial handler needs to be
    added for this case. Using a catch all `_` case will mean that a new case gets added, and its not a compile error
    for that function.
- Don't use `RecordWildCards`
- Use `EitherT/ExceptT` over exceptions.
  - If you have a handler for a particular exception type, and that function changes under your feet and decides not to throw that exception
    anymore (and the type is kept around because other functions still throw it), then your code will still compile just fine.
  - If the function is changed so that it throws a new kind of exception, again your code will continue to compile fine,
    and this new exception will be handled with your most generic handler, which may not be appropriate. The type system
    will not be able to help you audit all the possible cases either.
  - `EitherT` will fail to compile if the error type is changed, if cases are added or removed.
      - It is true that this still does not catch all possible changes that can happen (for e.g. if the error type is a general type used by
        other functions too, then if a function stops returning a particular case, it might not reault in a change in the type because another function
        that share the type may still return that case)
      - However, it still causes compile time errors for more interface changes than exceptions do.
      - Hence `EitherT/ExceptT` over exceptions all the time please.
      - Don't interpret `EitherT e IO a` as saying that all exceptions have been caught though please.
          - This type is explicitly saying that exceptions characterised by `e` have been caught, there might still
            be others thrown by `IO`, thats what it means when you see `IO` or `MonadThrow/MonadCatch` in a type signature.
          - Asynchronous exceptions will always be a thing and cannot/should not ever be caught into `EitherT/ExceptT`.
              - This will break the mechanism by which they stop other threads.
          - Often times though, we will write our code such that `e` contains cases for all the exceptions we care about,
            and we may go without having explicit handlers for any exceptions that get thrown by `IO`
            (as long we we use functions like `bracket'`).

## Compiler warnings

- `-Wall` always
- `-Werror` always
- `-fno-warn-orphans` never please, *maybe* when defining `Arbitrary` instances in test code for types that are in the prod source.
    - Use [`hedgehog`](http://hackage.haskell.org/package/hedgehog) over `quickcheck` it removes the need and temptation for
      `Arbitrary` instances and provides better support for shrinking.

## Testing

- plain quickcheck/hedgehog. Its all you really need, the frills that things like `tasty` and `hspec` and its ilk introduce are nice, but are just another source of potential problems.
- properties always
- split tests that do IO from tests that just run pure code.
    - `cabal` allows you to have multiple test suites, have one for pure code and one for IO
    - this is just so that it is easier to work offline, you have a test suite that doesnt require any databases, doesnt talk to AWS or do any kind of networking etc...
        - Also helps when there is an AWS outage or something similar
    - Call the pure test quite `test` (with a main file `test.hs`)
    - Call the io test quite `test-io` (with a main file `test-io.hs`)

- pure test property module naming convention: `Test.Project.<Prod.Module>` tests `Project.<ProdModule>`
- IO test property module naming convention: `Test.Project.IO.<Prod.Module>` tests `Project.<ProdModule>`
- gens and arbitraries: `Test.Property.Arbitrary`
- any code for things like helpers or "utilities" can go in the test modules, or they can be shared in some arbitrarily named module in `Test.Project.Util.*`

## Uniformity

**Apart from what is already mentioned above**:

 - Uniform, predictable looking code is great, but...

 - Use your own judgement (unless it contradicts whats already mentioned in the above document)
     - write code how you want
     - we have more important things to worry about.
     - If you automatically write something in a particular way, just do it (dont abuse this line pls).
     - This document is intended to avoid things that lead to bugs (yes, even the import/export list sections),
       if it doesnt appear in this document, and your judgement really believes its just a matter of aesthetics, then just do it your way.

 - Consider (to a certain extent) preserving the style that has gone before you in a particular library / project.
     - This is not a must just think about everyone else
     - Just reduces the noise in PRs, and lets people express themselves more naturally in their code.

 - Don't nitpick this sort of thing on PRs
     - (pointing out accidental whitespace/indentation is ok, but not "this doesn't look like I would write it". Yep it is different, but different is better than having a bunch of discussion / yaks / frustration about the inconsequential. Trust everyone else.

In summary, I would prefer not to say this but I am going to make my position clear: **don't   stress about   whitespace -  stress     about bugs**. (Some of the above style guidelines are more about avoiding a certain class of bugs)

## Tooling

The `stylish-haskell` tool provides a sensible way of formatting code. 
