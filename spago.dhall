{ name = "hyrule"
, dependencies =
  [ "arrays"
  , "avar"
  , "control"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "filterable"
  , "foldable-traversable"
  , "free"
  , "functors"
  , "js-timers"
  , "maybe"
  , "newtype"
  , "now"
  , "ordered-collections"
  , "prelude"
  , "refs"
  , "st"
  , "tuples"
  , "unsafe-coerce"
  , "unsafe-reference"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, license = "BSD-3-Clause"
, repository = "https://github.com/mikesol/purescript-hyrule"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
