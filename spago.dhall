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
  , "effect"
  , "either"
  , "eth-core"
  , "exceptions"
  , "foldable-traversable"
  , "homogeneous"
  , "identity"
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
  , "unfoldable"
  , "web3"
  ]
, packages = ./packages.dhall
, sources = [ "purs/src/**/*.purs", "test/**/*.purs" ]
}
