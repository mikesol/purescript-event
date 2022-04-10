{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "exists"
  , "filterable"
  , "foldable-traversable"
  , "js-timers"
  , "maybe"
  , "now"
  , "ordered-collections"
  , "prelude"
  , "profunctor"
  , "psci-support"
  , "refs"
  , "safe-coerce"
  , "tuples"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
