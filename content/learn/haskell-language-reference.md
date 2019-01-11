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
- inspired by a paper - http://strictlypositive.org/Idiom.pdf
- applicative has a bunch of laws (in category theory)
- if something implements applicative it should follow these laws
- it is possible to implement without the laws but it isn't a valid instance
- none of the laws are checked by the type system but we can check them with tests (ie property tests)

Brian: Functor -> Monoid -> Monad https://www.youtube.com/watch?v=ZhuHCtR3xq8
Relationship: Applicative-Monoid https://www.youtube.com/watch?v=RtYWKG_zZrM

- http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html (use pictures but descriptions about wrapping are incorrect/needs better descriptive language)

# string
String has exactly one use, and that’s showing Hello World in tutorials. For all other uses, Text is what people should be using.
