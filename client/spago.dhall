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
    , "decimals"
    , "effect"
    , "foldable-traversable"
    , "foreign"
    , "generics-rep"
    , "halogen"
    , "js-date"
    , "now"
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
