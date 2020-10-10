{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let override =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { "package-name" =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , "package-name" =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ],
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
  }
-------------------------------
-}


let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201007/packages.dhall sha256:35633f6f591b94d216392c9e0500207bb1fec42dd355f4fecdfd186956567b6b

let overrides = {=}

let postgresql-client =
  { dependencies =
      [ "aff"
      , "arrays"
      , "bifunctors"
      , "bytestrings"
      , "datetime"
      , "decimals"
      , "effect"
      , "either"
      , "exceptions"
      , "foldable-traversable"
      , "foreign"
      , "foreign-generic"
      , "foreign-object"
      , "js-date"
      , "lists"
      , "maybe"
      , "newtype"
      , "nullable"
      , "prelude"
      , "transformers"
      , "tuples"
      ]
  , repo =
      "https://github.com/rightfold/purescript-postgresql-client.git"
  , version =
      "v3.0.0"
  }

let selda =
  { dependencies =
      [ "console"
      , "exists"
      , "heterogeneous"
      , "lists"
      , "node-sqlite3"
      , "postgresql-client"
      , "prelude"
      , "simple-json"
      , "strings"
      , "test-unit"
      , "transformers"
      , "variant"
      , "prettyprinter"
      ]
  , repo =
      "https://github.com/Kamirus/purescript-selda.git"
  , version =
      "master"
  }

let prettyprinter = 
  { dependencies = 
      [ "prelude"
      , "unfoldable"
      , "random"
      , "ansi"
      , "console"
      ]
  , repo =
      "https://github.com/Kamirus/purescript-prettyprinter.git"
  , version = 
      "master"
  }

let routing-duplex-variant = 
  { dependencies =
      [ "routing-duplex"
      ]
  , repo = 
      "https://github.com/paluh/purescript-routing-duplex-variant.git"
  , version = 
      "master"
  }

let homogeneous =
  { dependencies =
      [ "assert"
      , "console"
      , "effect"
      , "foreign-object"
      , "psci-support"
      , "record-extra"
      , "typelevel-eval"
      , "variant"
      ]
  , repo =
      "https://github.com/paluh/purescript-homogeneous.git"
  , version =
      "master"
  }

let polyform =
  { dependencies =
      [ "newtype"
      , "ordered-collections"
      , "variant"
      , "profunctor"
      , "invariant"
      , "foreign-object"
      , "record"
      , "run"
      , "transformers"
      , "generics-rep"
      , "validation"
      , "foreign"
      ]
  , repo =
      "https://github.com/paluh/purescript-polyform.git"
  , version =
      "master"
  }

let polyform-batteries =
  { dependencies =
      [ "polyform"
      , "argonaut"
      , "prelude"
      , "affjax"
      , "numbers"
      ]
  , repo =
      "https://github.com/lambdaterms/purescript-polyform-batteries.git"
  , version =
      "master"
  }

let typelevel-eval =
  { dependencies =
      [ "prelude"
      , "typelevel-prelude"
      , "tuples"
      , "unsafe-coerce"
      , "leibniz"
      ]
  , repo =
      "https://github.com/natefaubion/purescript-typelevel-eval.git"
  , version =
      "master"
  }

let literal =
    { dependencies =
      [ "assert"
      , "effect"
      , "console"
      , "integers"
      , "numbers"
      , "partial"
      , "psci-support"
      , "unsafe-coerce"
      , "typelevel-prelude"
      ]
    , repo =
      "https://github.com/jvliwanag/purescript-literal.git"
    , version =
      "master"
   }

let oneof =
  { dependencies =
     [ "assert"
     , "console"
     , "effect"
     , "foreign"
     , "foreign-object"
     , "literal"
     , "maybe"
     , "newtype"
     , "proxy"
     , "psci-support"
     , "tuples"
     , "unsafe-coerce"
     ]
  , repo =
      "https://github.com/jvliwanag/purescript-oneof.git"
  , version =
      "master"
  }
let undefined-is-not-a-problem = ../purescript-undefined-is-not-a-problem/spago.dhall as Location
--   { dependencies =
--     [ "effect"
--     , "foreign"
--     , "prelude"
--     , "typelevel-prelude"
--     , "unsafe-coerce"
--     ]
--   , repo =
--       "https://github.com/paluh/purescript-undefined-is-not-a-problem.git"
--   , version =
--       "master"
--   }

let additions =
  { selda = selda
  -- , polyform = polyform
  -- , polyform-batteries = polyform-batteries
  , homogeneous = homogeneous
  , literal = literal
  , oneof = oneof
  , polyform = ../polyform/spago.dhall as Location
  , polyform-batteries = ../polyform-batteries/spago.dhall as Location
  , postgresql-client = postgresql-client
  , prettyprinter = prettyprinter
  , routing-duplex-variant = routing-duplex-variant
  , typelevel-eval = typelevel-eval
  , undefined-is-not-a-problem = undefined-is-not-a-problem
  }

let overrides =
  { httpure = upstream.httpure // { version = "master" }}

in  upstream // overrides // additions
