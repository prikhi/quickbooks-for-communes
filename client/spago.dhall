{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "quickbooks-for-communes"
, dependencies =
    [ "affjax"
    , "argonaut-codecs"
    , "argonaut-core"
    , "console"
    , "effect"
    , "foreign"
    , "generics-rep"
    , "halogen"
    , "ordered-collections"
    , "psci-support"
    , "routing"
    , "st"
    , "transformers"
    , "validation"
    ]
, packages =
    ./packages.dhall
}
