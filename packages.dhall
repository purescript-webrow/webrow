let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201007/packages.dhall sha256:35633f6f591b94d216392c9e0500207bb1fec42dd355f4fecdfd186956567b6b

let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let overrides = {=}


let homogeneous = mkPackage
  [ "assert", "console", "effect", "foreign-object", "psci-support"
  , "record-extra", "typelevel-eval", "variant"
  ]
  "https://github.com/paluh/purescript-homogeneous.git"
  "master"

let postgresql-client = mkPackage
  [ "aff", "arrays", "bifunctors", "bytestrings", "datetime", "decimals", "effect"
  , "either", "exceptions", "foldable-traversable", "foreign", "foreign-generic"
  , "foreign-object", "js-date", "lists", "maybe", "newtype", "nullable", "prelude"
  , "string-parsers", "transformers", "tuples"
  ]
  "https://github.com/rightfold/purescript-postgresql-client.git"
  -- "v3.1.0"
  "pool-query"

let polyform = mkPackage
  [ "newtype", "ordered-collections", "variant", "profunctor", "invariant", "foreign-object"
  , "record", "run", "transformers", "generics-rep", "validation", "foreign"
  ]
  "https://github.com/paluh/purescript-polyform.git"
  "master"

let polyform-batteries-core = mkPackage
  [ "polyform", "argonaut", "prelude", "affjax", "numbers" ]
  "https://github.com/purescript-polyform/batteries-core.git"
  "master"

let polyform-batteries-env = mkPackage
  [ "polyform-batteries-core" , "argonaut" , "prelude" , "affjax" , "numbers" ]
  "https://github.com/purescript-polyform/batteries-env.git"
  "master"

let polyform-batteries-urlencoded = mkPackage
  [ "polyform-batteries-core", "argonaut", "prelude", "affjax", "numbers" ]
  "https://github.com/purescript-polyform/batteries-urlencoded.git"
  "master"

let prettyprinter = mkPackage
  [ "prelude", "unfoldable", "random", "ansi", "console" ]
  "https://github.com/Kamirus/purescript-prettyprinter.git"
  "master"

let routing-duplex-variant = mkPackage
  [ "routing-duplex" ]
  "https://github.com/paluh/purescript-routing-duplex-variant.git"
  "master"

let resourcet = mkPackage
  [ "aff", "ordered-collections", "refs", "transformers" ]
  "https://github.com/paluh/purescript-resourcet.git"
  "master"

let selda = mkPackage
  [ "console", "exists", "heterogeneous", "lists", "node-sqlite3", "postgresql-client"
  , "prelude", "simple-json", "strings", "test-unit", "transformers", "variant", "prettyprinter"
  ]
  "https://github.com/Kamirus/purescript-selda.git"
  "scope-as-backend-with-new-pg-client"

let typelevel-eval = mkPackage
  [ "prelude", "typelevel-prelude", "tuples", "unsafe-coerce", "leibniz" ]
  "https://github.com/natefaubion/purescript-typelevel-eval.git"
  "master"

let undefined-is-not-a-problem = ../undefined-is-not-a-problem/spago.dhall as Location
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
  { homogeneous
  , polyform = ../polyform/spago.dhall as Location
  , polyform-batteries-core = ../polyform-batteries/spago.dhall as Location
  , polyform-batteries-env = ../batteries-env/spago.dhall as Location
  , polyform-batteries-json = ../batteries-json/spago.dhall as Location
  , polyform-batteries-urlencoded = ../batteries-urlencoded/spago.dhall as Location
  , postgresql-client = ../postgresql-client/spago.dhall as Location
  , prettyprinter
  , resourcet
  , routing-duplex-variant
  , typelevel-eval
  , selda = ../selda/spago.dhall as Location
  , undefined-is-not-a-problem
  }

let overrides =
  { httpure = upstream.httpure // { version = "master" }}

in  upstream // overrides // additions
