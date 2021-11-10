let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211030/packages.dhall sha256:5cd7c5696feea3d3f84505d311348b9e90a76c4ce3684930a0ff29606d2d816c

let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let homogeneous = mkPackage
  [ "assert", "console", "effect", "foreign-object", "psci-support"
  , "record-extra", "typelevel-eval", "variant"
  ]
  "https://github.com/paluh/purescript-homogeneous.git"
  "v0.3.0"

let js-uri = mkPackage
  [ "assert", "effect", "functions", "maybe", "prelude" ]
  "https://github.com/srghma/purescript-js-uri.git"
  "d25d83390ba9cf948f46695b55a5511895b0068c"

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
  "v0.1.0"

let postgresql-client = mkPackage
  [ "aff", "arrays", "bifunctors", "bytestrings", "datetime", "decimals", "effect"
  , "either", "exceptions", "foldable-traversable", "foreign", "foreign-generic"
  , "foreign-object", "js-date", "lists", "maybe", "newtype", "nullable", "prelude"
  , "string-parsers", "transformers", "tuples"
  ]
  "https://github.com/rightfold/purescript-postgresql-client.git"
  "v4.0.0-pre"


let prettyprinter = mkPackage
  [ "prelude", "unfoldable", "random", "ansi", "console" ]
  "https://github.com/Kamirus/purescript-prettyprinter.git"
  "master"

let resourcet = mkPackage
  [ "aff", "ordered-collections", "refs", "transformers" ]
  "https://github.com/robertdp/purescript-resourcet.git"
  "2183bac0f1a528a5d6cdddb4fa223c4a8b9bb604"

let routing-duplex-variant = mkPackage
  [ "routing-duplex" ]
  "https://github.com/paluh/purescript-routing-duplex-variant.git"
  "v0.1.0"

let run-streaming = mkPackage
  [ "prelude", "run" ]
  "https://github.com/paluh/purescript-run-streaming.git"
  "master"

let selda = mkPackage
  [ "console", "exists", "heterogeneous", "lists", "node-sqlite3", "postgresql-client"
  , "prelude", "simple-json", "strings", "test-unit", "transformers", "variant", "prettyprinter"
  ]
  "https://github.com/paluh/purescript-selda.git"
  "postgresql-client-v4.0.0"

let typelevel-eval = mkPackage
  [ "prelude", "typelevel-prelude", "tuples", "unsafe-coerce", "leibniz" ]
  "https://github.com/natefaubion/purescript-typelevel-eval.git"
  "polykinds"

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

in  upstream
  with
    httpure = upstream.httpure // { version = "6ce52417f79c95c9fac413189825f35472c8f937" }
  with
    homogeneous = homogeneous
  with
    js-unsafe-stringify = ../purescript-js-unsafe-stringify/spago.dhall as Location
  with
    js-uri = js-uri
  with
    logging-journald = ../purescript-logging-journald/spago.dhall as Location
  with
    media-types = upstream.media-types // { version = "4c685071074065506403197b7a5f22eb661ff17c" }
  with
    polyform = ../polyform/spago.dhall as Location
  with
    polyform-batteries-core = ../batteries-core/spago.dhall as Location
  with
    polyform-batteries-env = ../batteries-env/spago.dhall as Location
  with
    polyform-batteries-json = ../batteries-json/spago.dhall as Location
  with
    polyform-batteries-urlencoded = ../batteries-urlencoded/spago.dhall as Location
  with
    postgresql-client = ../postgresql-client/spago.dhall as Location
  with
    prettyprinter = prettyprinter
  with
    resourcet = resourcet
  with
    routing-duplex-variant = routing-duplex-variant
  with
    run-streaming = run-streaming
  with
    typelevel-eval = typelevel-eval
  with
    selda = ../selda/spago.dhall as Location
  with
    smolder = mkPackage
    [ "bifunctors"
    , "catenable-lists"
    , "free"
    , "js-uri"
    , "ordered-collections"
    , "prelude"
    , "strings"
    , "test-unit"
    , "transformers"
    , "tuples"
    ]
    "https://github.com/nsaunders/purescript-smolder.git"
    "ps-0.14"
