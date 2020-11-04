{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "audiocarrier"
, dependencies =
  [ "b64"
  , "console"
  , "crypto"
  , "effect"
  , "homogeneous"
  , "httpure"
  , "logging-journald"
  , "optparse"
  , "polyform"
  , "polyform-batteries"
  , "polyform-batteries-env"
  , "postgresql-client"
  , "profunctor-lenses"
  , "psci-support"
  , "record"
  , "record-extra"
  , "routing-duplex"
  , "routing-duplex-variant"
  , "run"
  , "run-streaming"
  , "selda"
  , "simple-jwt"
  , "smolder"
  , "spec"
  , "strings"
  , "string-parsers"
  , "typelevel-eval"
  , "undefined-is-not-a-problem"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
