{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "avar"
  , "chanterelle"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "errors"
  , "eth-core"
  , "exceptions"
  , "foldable-traversable"
  , "homogeneous"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-process"
  , "parallel"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "random"
  , "strings"
  , "tagged"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "unsafe-coerce"
  , "web3"
  ]
, packages = ./packages.dhall
, sources = [ "purs/src/**/*.purs", "test/**/*.purs" ]
}
