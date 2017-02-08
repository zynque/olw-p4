module Tests.Olw.Document.BuildTests exposing(..)

import Test exposing (..)
import Expect

import Olw.Document.Document as Document exposing (..)
import Olw.Document.Detached as Detached exposing (..)
import Olw.Document.Build as Build exposing (..)
import Olw.Document.WorkingDocument as WorkingDocument exposing (..)

rootNodeData = "data"
expectedNode = VersionedNode {version = 0, node = Node {data = rootNodeData, childIds = []}}

buildTest : Test
buildTest =
  describe "build" [
    test "beginWorkingDocument" <|
      \() ->
        let wd = Build.beginWorkingDocument rootNodeData
        in WorkingDocument.getVersionedNode 0 wd
        |> Expect.equal (Just expectedNode)
  ]
