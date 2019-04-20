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
    , "either"
    , "foldable-traversable"
    , "foreign"
    , "foreign-object"
    , "fuzzy"
    , "generics-rep"
    , "halogen"
    , "halogen-nselect"
    , "js-date"
    , "now"
    , "ordered-collections"
    , "psci-support"
    , "rationals"
    , "routing"
    , "st"
    , "transformers"
    , "validation"
    ]
, packages =
    ./packages.dhall
}
