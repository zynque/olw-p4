module Tests.Olw.Document.BuildTests exposing(..)

import Test exposing (..)
import Expect

import Olw.Document.Document as Document exposing (..)
import Olw.Document.Detached as Detached exposing (..)
import Olw.Document.Build as Build exposing (..)
import Olw.Document.WorkingDocument as WorkingDocument exposing (..)


buildTest : Test
buildTest =
  describe "build" [
    test "beginWorkingDocument" <|
      \() ->
        let rootNodeData = "data"
            wd = Build.beginWorkingDocument rootNodeData
            expectedNode = {
              version = 0,
              data = rootNodeData,
              childIds = []
            }
        in WorkingDocument.getVersionedNode 0 wd
        |> Expect.equal (Just expectedNode)
  ]
