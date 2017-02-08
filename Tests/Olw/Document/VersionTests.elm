module Tests.Olw.Document.VersionTests exposing(..)

import Test exposing (..)
import Expect

import Olw.Document.Document exposing (..)
import Olw.Document.WorkingDocument as WorkingDocument exposing (..)
import Olw.Document.Detached as Detached exposing (..)
import Olw.Document.Build as Build exposing (..)
import Olw.Document.Edit exposing (..)

-- To determine a common ancestor to use when merging two versions, we note that
-- version history forms a DAG (Directed Acyclic Graph), and we use the
-- LSCA (Lowest Single Common Ancestor), as described in [*].
-- Note that a node in the DAG can have zero, one, or at most two parents.
-- This fact may simplify our algorithms.
-- 
-- We maintain an LSA (Lowest Single Ancestor) tree along side the version tree.
-- (See Def 3 & 4)
-- Lemma 3 shows that the LSCA can be determined by finding the
-- lowest common ancestor of the two nodes in the corresponding LSA tree.
--
-- * Fischer, Johannes, and Daniel H. Huson.
--   "New common ancestor problems in trees and directed acyclic graphs."
--   Information processing letters 110.8-9 (2010): 331-335.

-- To find the common ancestor in the LSA tree
-- we find the path in this tree to
-- each of the nodes to be merged, and compare the paths side-by-side
-- starting from the root, until they diverge. This is O(h) where h is the
-- height of the Lowest Single Ancestor tree.
-- This could potentially be improved to O(log h) using the techniques
-- outlined by Edward Kmett here:
--
-- https://www.schoolofhaskell.com/user/edwardk/online-lca
--
-- For now, though, O(h) should suffice

-- We need an online algorithm for maintaining the LSA tree.
-- Adding a new version with a single parent is trivial. There are no diverging paths, so
-- said node has the same parent in the LSA tree. The slightly harder problem is when two
-- nodes are merged - the new child then has both nodes as parents.

-- let v and w be two nodes in the version tree. When we merge them, we generate
-- a new node u, whose parents are v and w. We must determine the lowest single ancestor of
-- this new node - that is the node l such that all paths from root to u go through l, and l is the lowest
-- such node. Because v and w are u's only parents, all paths to u must go through either v or w. Therefore
-- the LSA of u must be the LSCA of v and w.


type Version t = Version {
    mergedFromNodeId: Maybe Int,
    --parentNodeId:     Maybe Int
    data:             t,
    lsaNodeId:        Maybe Int
  }

buildRootVersionNode d = Version {
    mergedFromNodeId = Nothing,
    --parentNodeId = Nothing,
    data = d,
    lsaNodeId = Nothing
  }

type alias WorkingVersionTree t = WorkingDocument (Version t)

--insertNode : DetachedNode tData -> Int -> Int ->
--             WorkingDocument tData -> Result String (WorkingDocument tData)

update : Int -> d -> WorkingDocument (Version d) -> Result String (WorkingDocument (Version d))
update parentNodeId data workingDocument =
  let version = Version {mergedFromNodeId = Nothing, data = data, lsaNodeId = Just parentNodeId}
      detachedNode = DetachedNode {data = version, children = []}
      index = 0
  in  insertNode detachedNode parentNodeId index workingDocument



initialVersionDoc = Build.beginWorkingDocument (buildRootVersionNode "0a")

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
               |> update 0 "1a"
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

versionTest : Test
versionTest =
  describe "Version" [
    test "update should extend branch with new version node" <|
      \() -> actualNode1
        |> Expect.equal (Ok (expectedNode1)),
    test "update should add new branch" <|
      \() -> actualNode2
        |> Expect.equal (Ok (expectedNode2))
  ]
