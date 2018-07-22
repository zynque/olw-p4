-- The Open Language Web manifesto


module Main exposing (..)


type alias Manifesto =
    { title : String
    , summary : String
    , principles : List TopLevelPrinciple
    }


type alias TopLevelPrinciple =
    { label : String
    , children : List OLWPrinciple
    }


type alias OLWPrinciple =
    { header : String
    , description : String
    , implementationHints : String
    }


manifesto : Manifesto
manifesto =
    { title = "The Open Language Web Manifesto"
    , summary = ""
    , principles = principles
    }


principles : List TopLevelPrinciple
principles =
    [ { label = "Frictionless & Open"
      , children =
            [ { header = "Open Source", description = "" }
            , { header = "Frictionless", description = "Every piece of creativity/productivity software should carry with it a full integrated development environment, so that the software can be rewritten from directly within the application itself." }
            , { header = "Inherit", description = "By default, applications built using this platform should inherit all the capabilities of the platform itself - thus upholding all the principles described here. Applications may chose to opt out of some or all of these features where it makes sense to do so." }
            ]
      }
    , { label = "Language Oriented"
      , children =
            [ { header = "Discoverablility", description = "Provide choices for the user (auto-complete, drop-down menus)" }
            , { header = "Immediate types", description = "" }
            , { header = "Programmable", description = "" }
            , { header = "Polysyntactic", description = "Allow multiple representations" }
            , { header = "Polysemantic", description = "Allow for multiple meanings/interpretations" }
            ]
      }
    , { label = "Web Based"
      , children =
            [ { header = "Online/offline", description = "Fully functional while offline, and synchronizes when on." }
            , { header = "Preserve History", description = "Every application should have an undo button. It is common sense for developers to use a source control system. So it should be for every productivity/creativity application. It should be integrated, so that history can be represented not just as side by side plain text, but as various representations as suits the application. A diff tool should be aware of the _meaning_ of the difference." }
            , { header = "Reactive", description = "The results of a change to source code should be made immediately visible via sample usages (similar to unit tests). Changes to libraries that your application depends on should pop up non-obtrusive notifications, indicating the severity and nature of the change. Opting into such a change should be as simple as a click of a button." }
            , { header = "Real Time Collaboration", description = "Allow users to edit the code interactively in real time." }
            ]
      }
    ]
