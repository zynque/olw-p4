-- The Open Language Web manifesto


module Main exposing (..)


type alias TopLevelPrinciple =
    { label : String
    , children : List OLWPrinciple
    }


type alias OLWPrinciple =
    { header : String
    , description : String
    }



-- "Every piece of creativity/productivity software should carry with it a full integrated development environment, so that the software can be rewritten from directly within the application itself."


principles : List TopLevelPrinciple
principles =
    [ { label = "Open & Transparent"
      , children = []
      }
    ]
