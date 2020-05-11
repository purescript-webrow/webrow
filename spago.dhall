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
    , "httpure"
    , "logging-journald"
    , "optparse"
    , "polyform-validators"
    , "postgresql-client"
    , "profunctor-lenses"
    , "psci-support"
    , "record"
    , "routing-duplex"
    , "routing-duplex-variant"
    , "run"
    , "selda"
    , "smolder"
    , "uuid"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
