---
description: ""
title: "Haskell language extensions"
draft: false
weight: 200
bref:
toc: true
---

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

