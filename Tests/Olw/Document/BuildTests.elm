module Tests.Olw.Document.BuildTests exposing (..)

import Expect
import Olw.Document.Build as Build exposing (..)
import Olw.Document.Detached as Detached exposing (..)
import Olw.Document.Document as Document exposing (..)
import Test exposing (..)


buildTest : Test
buildTest =
    describe "build"
        [ test "beginWorkingDocument" <|
            \() ->
                let
                    rootNodeData =
                        "data"

                    doc =
                        Build.beginDocument rootNodeData

                    expectedNode =
                        { version = 0
                        , data = rootNodeData
                        , childIds = []
                        , parentId = Nothing
                        }
                in
                Document.getNode 0 doc
                    |> Expect.equal (Just expectedNode)
        ]
