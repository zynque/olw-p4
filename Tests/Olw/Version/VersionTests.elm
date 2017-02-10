module Tests.Olw.Version.VersionTests exposing(..)

import Test exposing (..)
import Expect

import Olw.Document.Document exposing (..)
import Olw.Document.WorkingDocument as WorkingDocument exposing (..)
import Olw.Document.Detached as Detached exposing (..)
import Olw.Document.Build as Build exposing (..)
import Olw.Version.Version as Version exposing (..)

initialVersionDoc = Build.beginWorkingDocument (Version.buildRootVersionNode "0a")

expectedVersion1 = Version {
    mergedFromNodeId = Nothing,
    data = "1a",
    lsaNodeId = Just 0
  }

expectedNode1 = VersionedNode {
  version = 0,
  node = Node {data = expectedVersion1, childIds = []}}

actualNode1 : Result String (VersionedNode (Version String))
actualNode1 = initialVersionDoc
               |> Version.update 0 "1a"
               |> Result.andThen ((WorkingDocument.getVersionedNode 1) >> (Result.fromMaybe "node not found")) 

expectedVersion2 = Version {
    mergedFromNodeId = Nothing,
    data = "1b",
    lsaNodeId = Just 0
  }

expectedNode2 = VersionedNode {
  version = 0,
  node = Node {data = expectedVersion2, childIds = []}}

actualNode2 : Result String (VersionedNode (Version String))
actualNode2 = initialVersionDoc
                |> update 0 "1a"
                |> Result.andThen (update 0 "1b")
                |> Result.andThen ((WorkingDocument.getVersionedNode 2) >> (Result.fromMaybe "node not found"))

largerExampleVersionTree =
  initialVersionDoc
    |> update 0 "1a"                   -- node 1
    |> Result.andThen (update 0 "1b")  -- node 2
    |> Result.andThen (update 2 "2a")  -- node 3
    |> Result.andThen (update 2 "2b")  -- node 4


lsca_1_3 =
  largerExampleVersionTree
    |> Result.andThen (getLsca 1 3 >> Result.fromMaybe "no common ancestor")

lsca_3_4 =
  largerExampleVersionTree
    |> Result.andThen (getLsca 3 4 >> Result.fromMaybe "no common ancestor")

versionTest : Test
versionTest =
  describe "Version" [

    test "update extends branch with new version node" <|
      \() -> actualNode1
        |> Expect.equal (Ok expectedNode1),
    test "update adds new branch" <|
      \() -> actualNode2
        |> Expect.equal (Ok expectedNode2),
    test "update extends branch further with correct lsa" <|
      \() -> largerExampleVersionTree
        |> Result.andThen (getVersion 4 >> Result.fromMaybe "no version")
        |> Result.andThen (\(Version {lsaNodeId}) -> Result.fromMaybe "no lsa" lsaNodeId)
        |> Expect.equal (Ok 2),

    test "getLastCommonElement returns last common element" <|
      \() -> getLastCommonElement [1, 2, 3, 4, 5] [1, 2, 3, 6, 7]
        |> Expect.equal (Just 3),
    test "getLastCommonElement returns Nothing when no common element" <|
      \() -> getLastCommonElement [1, 2, 3] [4, 5]
        |> Expect.equal Nothing,
    test "getLastCommonElement returns Nothing for empty lists" <|
      \() -> getLastCommonElement [] []
        |> Expect.equal Nothing,

    test "lsaPathFromRootTo element returns correct path (0 1)" <|
      \() -> largerExampleVersionTree |> Result.map (lsaPathFromRootTo 1)
        |> Expect.equal (Ok [0, 1]),
    test "lsaPathFromRootTo element returns correct path (0 2 4)" <|
      \() -> largerExampleVersionTree |> Result.map (lsaPathFromRootTo 4)
        |> Expect.equal (Ok [0, 2, 4]),

    test "determine lowest singular common ancestor of two nodes with same parent" <|
      \() -> lsca_3_4
        |> Expect.equal (Ok 2),
    test "determine lowest singular common ancestor of two nodes further apart" <|
      \() -> lsca_1_3
        |> Expect.equal (Ok 0),

    test "lsa of merged node is lsca of parents" <|
      \() -> largerExampleVersionTree
        |> Result.andThen (merge 1 3 "3a")
        |> Result.andThen (getVersion 5 >> Result.fromMaybe "no version")
        |> Result.andThen (\(Version {lsaNodeId}) -> Result.fromMaybe "no lsa" lsaNodeId)
        |> Expect.equal (Ok 0)

  ]