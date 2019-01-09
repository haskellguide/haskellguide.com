---
description: ""
title: "Haskell language reference"
draft: false
weight: 200
bref:
toc: true
---

# applicatives
- monads were added before applicative
- applicative is a superclass of monad
- there's a paper
- applicative has a bunch of laws (in category theory)
- if something implements applicative it should follow these laws
- it is possible to do without the laws but it isn't a valid instance
- none of the laws are checked by the type system but we can check them with tests (ie property tests)
