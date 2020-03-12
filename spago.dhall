{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "console"
    , "debug"
    , "dotlang"
    , "effect"
    , "generic-graphviz"
    , "generics-rep"
    , "globals"
    , "graphviz"
    , "ordered-collections"
    , "parsing"
    , "prelude"
    , "psci-support"
    , "react-basic"
    , "react-basic-hooks"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
