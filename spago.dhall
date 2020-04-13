{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "audiocarrier"
, dependencies =
    [ "console"
    , "effect"
    , "httpure"
    , "logging-journald"
    , "optparse"
    , "postgresql-client"
    , "psci-support"
    , "run"
    , "selda"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
