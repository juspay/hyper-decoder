let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220527/packages.dhall
        sha256:15dd8041480502850e4043ea2977ed22d6ab3fc24d565211acde6f8c5152a799

in  upstream
    with foreign-generic =
          { dependencies =
            [ "arrays"
            , "assert"
            , "bifunctors"
            , "console"
            , "control"
            , "effect"
            , "either"
            , "exceptions"
            , "foldable-traversable"
            , "foreign"
            , "foreign-object"
            , "identity"
            , "lists"
            , "maybe"
            , "newtype"
            , "partial"
            , "prelude"
            , "record"
            , "strings"
            , "transformers"
            , "tuples"
            , "typelevel-prelude"
            , "unsafe-coerce"
            ]
          , repo = "ssh://git@ssh.bitbucket.juspay.net/fram/purescript-foreign-generic.git"
          , version = "v12.0.0"
          }