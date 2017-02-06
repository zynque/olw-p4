import Test exposing (..)
import Expect
import Test.Runner.Html
import Array exposing (Array)

import Tests.Olw.Document.WorkingDocumentTests exposing(..)


main : Test.Runner.Html.TestProgram
main =
  [ workingDocumentTest
  ] |> concat |> Test.Runner.Html.run
