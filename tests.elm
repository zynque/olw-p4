import Test exposing (..)
import Expect
import Test.Runner.Html
import Array exposing (Array)

import Tests.Olw.Document.DocumentTests exposing(..)
import Tests.Olw.Document.BuildTests exposing(..)
import Tests.Olw.Version.VersionTests exposing(..)


main : Test.Runner.Html.TestProgram
main =
  [ documentTest,
    buildTest,
    versionTest
  ] |> concat |> Test.Runner.Html.run
