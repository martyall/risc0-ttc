let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.2-20210629/packages.dhall sha256:534c490bb73cae75adb5a39871142fd8db5c2d74c90509797a80b8bb0d5c3f7b

let overrides = {=}

let additions =
      { web3 =
        { dependencies =
          [ "aff"
          , "avar"
          , "console"
          , "coroutines"
          , "coroutine-transducers"
          , "debug"
          , "effect"
          , "errors"
          , "eth-core"
          , "foreign"
          , "foreign-generic"
          , "fork"
          , "free"
          , "heterogeneous"
          , "identity"
          , "parsing"
          , "partial"
          , "profunctor-lenses"
          , "psci-support"
          , "tagged"
          , "transformers"
          , "typelevel-prelude"
          , "variant"
          ]
        , repo = "https://github.com/f-o-a-m/purescript-web3"
        , version = "0cd07f8"
        }
      , web3-generator =
          { dependencies =
            [ "ansi"
            , "argonaut"
            , "console"
            , "effect"
            , "errors"
            , "eth-core"
            , "fixed-points"
            , "mkdirp"
            , "node-fs-aff"
            , "ordered-collections"
            , "prelude"
            , "psci-support"
            , "record-extra"
            , "string-parsers"
            , "web3"
            , "yargs"
            ]
          , repo = "https://github.com/f-o-a-m/purescript-web3-generator"
          , version = "211e56d"
          }
      , eth-core =
        { dependencies =
            [ "argonaut"
            , "bytestrings"
            , "console"
            , "debug"
            , "effect"
            , "foreign-generic"
            , "ordered-collections"
            , "parsing"
            , "prelude"
            , "psci-support"
            , "ring-modules"
            , "simple-json"
            ]
        , repo =
            "https://github.com/f-o-a-m/purescript-eth-core.git"
        , version =
            "v8.0.0"
        }
      , coroutine-transducers =
        { dependencies =
            [ "aff"
            , "coroutines"
            , "effect"
            , "maybe"
            , "psci-support"
            ]
        , repo =
            "https://github.com/blinky3713/purescript-coroutine-transducers"
        , version =
            "v1.0.0"
        }
      , solc =
        { dependencies =
          [ "aff"
          , "argonaut"
          , "console"
          , "effect"
          , "node-path"
          , "prelude"
          , "psci-support"
          , "web3"
          ]
        , repo =
            "https://github.com/f-o-a-m/purescript-solc"
        , version =
            "v3.0.0"
        }
      , mkdirp =
        { dependencies =
            [ "console"
            , "effect"
            , "either"
            , "exceptions"
            , "functions"
            , "node-fs"
            , "nullable"
            , "prelude"
            , "psci-support"
            ]
        , repo =
            "https://github.com/f-o-a-m/purescript-mkdirp"
        , version =
            "v1.0.0"
        }
      , tagged =
        { dependencies =
            [ "identity"
            , "profunctor"
            ]
        , repo =
            "https://github.com/kejace/purescript-tagged"
        , version =
            "v0.14"
        }
      , chanterelle =
        { dependencies =
          [ "console"
          , "debug"
          , "effect"
          , "foreign-object"
          , "logging"
          , "mkdirp"
          , "node-process"
          , "optparse"
          , "prelude"
          , "psci-support"
          , "solc"
          , "validation"
          , "web3"
          , "web3-generator"
          ]
        , repo = "https://github.com/f-o-a-m/chanterelle"
        , version = "v6.0.0"
        }
      , dodo-printer =
        { dependencies =
          [ "ansi", "foldable-traversable", "lists", "maybe", "strings" ]
        , repo = "https://github.com/natefaubion/purescript-dodo-printer.git"
        , version = "v2.1.0"
        }
      , language-cst-parser =
        { dependencies =
          [ "arrays"
          , "const"
          , "effect"
          , "either"
          , "foldable-traversable"
          , "free"
          , "functors"
          , "maybe"
          , "numbers"
          , "ordered-collections"
          , "strings"
          , "transformers"
          , "tuples"
          , "typelevel-prelude"
          ]
        , repo =
            "https://github.com/natefaubion/purescript-language-cst-parser.git"
        , version = "v0.9.1"
        }
      , tidy =
        { dependencies =
          [ "arrays"
          , "dodo-printer"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "language-cst-parser"
          , "strings"
          , "tuples"
          ]
        , repo =
            "https://github.com/natefaubion/purescript-tidy.git"
        , version = "v0.5.3"
        }
      , language-cst-codegen =
        { dependencies =
          [ "aff"
          , "ansi"
          , "arrays"
          , "avar"
          , "bifunctors"
          , "console"
          , "control"
          , "dodo-printer"
          , "effect"
          , "either"
          , "enums"
          , "exceptions"
          , "filterable"
          , "foldable-traversable"
          , "free"
          , "identity"
          , "integers"
          , "language-cst-parser"
          , "lazy"
          , "lists"
          , "maybe"
          , "newtype"
          , "node-buffer"
          , "node-child-process"
          , "node-fs-aff"
          , "node-path"
          , "node-process"
          , "node-streams"
          , "ordered-collections"
          , "parallel"
          , "partial"
          , "posix-types"
          , "prelude"
          , "record"
          , "safe-coerce"
          , "strings"
          , "tidy"
          , "transformers"
          , "tuples"
          , "type-equality"
          , "unicode"
          ]
        , repo =
            "https://github.com/natefaubion/purescript-tidy-codegen.git"
        , version = "v1.1.1"
        }
      }

in  upstream // overrides // additions
