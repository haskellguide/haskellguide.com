---
description: ""
title: "Serialization"
draft: false
weight: 200
bref:
toc: true
---

# libraries
- https://kowainik.github.io/posts/2019-01-14-tomland

# domain types

The problem of how to effectively isolate parts of a system, and provide a suitable maintainable solution to
serialising Domain Types.

First we accept and agree that we want a clear boundary between domains (contexts) and we want to provide increasing levels of trust as we move from a serialised document into the core domain types used in an application.

The general structure conceptually would look like this when serialising.

 1. Domain Type (also known as Domain Object in OO) exists
 2. Structured transformation from this DT into a Data Transfer Type (also know as Data Transfer Object again from OO).
    This stage could be futher broken up into multiple stages, where there might be some business transformations and
    then more DTT specific formatting / escaping transformations.
 3. Domain Transfer Type represents a suitable type for serialisation to something like JSON or XML
 4. Actual serialisation of the DTT, this stage is closely tied to the serialisation format.
 5. Resulting serialisation format

```

   +----------------------------------------+
   |                                        |
   |                     Domain Boundary    |        ---> To downstream context
   |                                        |
   |                                        |
   | +------------+    +-----------+    +---+------+        +-----------+        +------------+
   | | Domain Type|--->| DT to DTT |--->| DTT Type |------->| Serialise |------->| JSON/XML   |
   | +------------+    +-----------+    +---+------+        +-----------+        +------------+
   |                                        |
   |                                        |
   |                                        |
   +----------------------------------------+

```

Each of the stages 2 and 4 can have different implementations but would typically be seen as functions or derived
functions based off the types. The specific implementations are interesting and to be cover later on, but the concept
and motivation is consistent.

Now going the other way from a serialised representation all the way into Domain Types would look like:

 1. Serialised representation from some network / IO source.
 2. Structured transformation from serialised format into a DTT. Here we would break this transformation into
    two distinct stages, first structural mapping from the specific format like JSON into the DTT. The examples
    being checking enum fields, date formats, string sizes. The second stage being more business style validations
    eg the date is supposed to be a birth date, is it in the right range? The guiding principle is:

    `validate each thing at the earliest possible step that makes sense,
     there are subjective words there but that's the point, it is supposed to
     frame the discussion, people should be discussing "earliest" and
     "makes sense" for each each case`

  3. Data Transfer Type represents a suitable type from the serialisation format. Following on from 2. there could
     be 2 different types here representing the inputs and outputs for the two stages of validation.

  4. Ending in a Domain Type.

```


                                           +---------------------------------------------------+
                                           |                                                   |
           From upstream context --->      |      Domain Boundary                              |
                                           |                                                   |
                                           |                                                   |
    +------------+    +------------+    +---+------+       +-----------+        +------------+ |
    | JSON/XML   |--->| Deserialise|--->| DTT Type |------>| DTT to DT |------->| Domain Type| |
    +------------+    +------------+    +---+------+       +-----------+        +------------+ |                                          |                                           |                                                   |
                                           |                                                   |
                                           |                                                   |
                                           |                                                   |
                                           +---------------------------------------------------+

```

Example
----------

Given we mostly speak Haskell, I will show an opinionated version of how to follow this pattern in that language using
our chosen tools.

``` haskell
-- | Core Domain Type representing a data feed
module Terminus.Core.Data where

data Source
  = RSS Text
  | ATOM Text

data Feed = Feed {
    feedId :: UUID
  , feedName :: Text
  , feedDate :: UTCTime
  , feedSource :: Source
  } derving (Eq, Show)
```

``` haskell
-- | Data Transfer Types with transforms from / to FeedDTT
module Terminus.DTT where

import qualified Terminus.Core.Data as D

data FeedDTT = FeedDTT {
    feedId :: Text
  , feedName :: Text
  , feedSource :: Source
  } derving (Eq, Show)

-- Various errors that might happen during validation
data Error = ....

validateFeedDTT :: FeedDTT -> Either [Error] Feed

transformFeedDTT :: Feed -> FeedDTT

```

``` haskell
-- | Validation functions from JSON to DTT with transforms from / to FeedJson
module Terminus.Validation where

validateFeed :: FeedJson -> Either [JSONError] FeedDTT

transform :: FeedDTT -> FeedJson
```


``` haskell
-- | Aeson based serialisation into JSON
module Terminus.Representation where

data FeedJSON = FeedJSON Feed

instance FromJSON FeedJson
  toJSON a = ....

instance ToJSON FeedJSON
   parseJSON a = ....

```

Example json output

``` json
{
  "feed-id" : "123"
, "feed-name" : "blarg"
, "feed-source" : "https://terminus.github.io/rss.xml"
}
```

Other options
----------

Geez that seems like a lot of code to write, you might say. Sure there are opportunities for reducing the code
that we could explore but this version is the minimal magic solution using just functions, types and a basic type class.

Things to explore in future to cut down code are:

 * template Haskell to generate serialisation instances
 * GHC Generics
 * lenses to simplify translations

Guiding Principles
----------

> Make invalid states unrepresentable &mdash; <cite>Yaron Minksy</cite>

> Validate each thing at the earliest possible step that makes sense &mdash; <cite>Dom De Re</cite>
