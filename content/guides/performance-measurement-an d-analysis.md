---
description: ""
title: "Performance measurement and analysis"
draft: false
weight: 400
bref:
toc: true
---

# Profiling Haskell

## Build Dependencies with Profiling enabled

With `cabal`:

``` shell
cabal install --ghc-option=-fprof-auto --only-dependencies --enable-tests --enable-library-profiling
```

With `mafia`, you don't really need to think about this you just build the main project with profiling
and it takes care of the dependencies.

## Configure main build for profiling

``` shell
cabal configure --enable-tests --ghc-option=-fprof-auto --enable-library-profiling --enable-profiling
```

The `prof-auto` flag enables *automatic cost centre* creation.

Or with `mafia`:

``` shell
./mafia build -p
```

## Running it

### Plain Time Profiling

``` shell
foo cmd args opts +RTS -p
```

### Standard Heap Profile

``` shell
foo cmd args opts +RTS -hc -p
```

### Heap Profile by Type

``` shell
foo cmd args opts +RTS -hy -p
```

### Heap Profile by Constructor

``` shell
foo cmd args opts +RTS -hd -p
```

### Biographical Heap Profile

This shows the cross section of heap objects in the following states over time:

-   The *lag* stage, which it the time between the creation and first use of an object
-   The *use* stage, the time between the first and last use of an object
-   The *drag* stage, the time from the final use until the last reference of the object is dropped
-   An object that is allocated but never actually used is in the *void* state.

``` shell
foo cmd args opts +RTS -hb -p
```

### Retainer Heap Profile Usage

After getting a biographical heap profile, it might be handy to try and work out which retainer is holding it. This can also be used in conjunction with the `-hc` flag to narrow it down to the right cost centre.

``` shell
foo cmd args opts +RTS -hr -p
```

### Rendering Time Profile

`profiteur` is a cool tool available on `hackage` that will render the `.prof` file in `html` basically allowing you to find the part of code taking up the inordinate amount of time just by continually double clicking on the biggest coloured box.

``` shell
profiteur foo.prof
```

## Rendering Heap Profiling Information

``` shell
hp2ps -c foo.hp
```
