{ name = "event"
, dependencies =
  [ "arrays"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "filterable"
  , "foldable-traversable"
  , "js-timers"
  , "maybe"
  , "monoid-extras"
  , "newtype"
  , "now"
  , "ordered-collections"
  , "prelude"
  , "profunctor"
  , "record"
  , "refs"
  , "st"
  , "tuples"
  , "unsafe-coerce"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
