{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "audiocarrier"
, dependencies =
    [ "console"
    , "crypto"
    , "effect"
    , "homogeneous"
    , "httpure"
    , "logging-journald"
    , "oneof"
    , "optparse"
    , "polyform"
    , "polyform-batteries"
    , "postgresql-client"
    , "profunctor-lenses"
    , "psci-support"
    , "record"
    , "record-extra"
    , "routing-duplex"
    , "routing-duplex-variant"
    , "run"
    , "selda"
    , "simple-jwt"
    , "smolder"
    , "strings"
    , "typelevel-eval"
    , "undefined-is-not-a-problem"
    , "uuid"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
