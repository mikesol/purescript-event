let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "test/**/*.purs" ]
        , dependencies = conf.dependencies # [ "aff", "spec", "profunctor", "js-date" ]
        , backend = "purs-backend-es build"
        }
