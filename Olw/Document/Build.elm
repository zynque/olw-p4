module Olw.Document.Build exposing (
    buildDocument
  )

import Array exposing (Array)
import String exposing (join)
import Olw.Document.Data exposing (..)
import Olw.Document.Detached as Detached exposing (..)
import Olw.Document.Document exposing (..)

emptyDocument = Document {rootId = 0, versionedNodes = Array.fromList []}

buildDocument : DetachedNode tData -> Document tData
buildDocument detached =
  addDetachedNode detached emptyDocument

-- addDetachedNode
-- recursive helper method takes doc of nodes added so far
-- and returns it with the new detached node & its children added
-- the rootId is the assigned id of the detached node

addDetachedNode : DetachedNode tData -> Document tData -> Document tData
addDetachedNode detachedNode rawDoc =
  let (Document {rootId, versionedNodes}) = rawDoc
  in case detachedNode of
    DetachedLeaf data ->
      let rootId = Array.length versionedNodes
          newNode = VersionedNode {version = 0, node = Leaf data}
          newNodes = Array.push newNode versionedNodes
      in Document {rootId = rootId, versionedNodes = newNodes}
    DetachedNode children ->
      let addChild detached (ids, doc) =
            let newDoc = addDetachedNode detached doc
                (Document {rootId, versionedNodes}) = newDoc
            in (rootId :: ids, newDoc)
          (childIdsRev, docWithChildren) = List.foldl addChild ([], rawDoc) children
          (Document {rootId, versionedNodes}) = docWithChildren
          newRootId = Array.length versionedNodes
          childIds = List.reverse childIdsRev
          newNode = VersionedNode {version = 0, node = Node {childIds = childIds}}
          newNodes = Array.push newNode versionedNodes
      in  Document {rootId = newRootId, versionedNodes = newNodes}


--blankNode = VersionedNode {
--        parentId = Nothing,
--        versionId = 0,
--        index = Nothing,
----        documentNode = InternalNode {childIndices = Array.fromList []}
--        documentNode = InternalNode {childIndices = []}
--      }

--buildDocument : DetachedNode tData -> VersionedDocument tData
--buildDocument detached =
--  let rawDoc = buildDocument detached
--      (Document {rootId, nodes}) = rawDoc
--      numNodes = Array.length nodes
--      blankNodes = Array.repeat numNodes blankNode
--      versionedNodes = addNodesToDocument nodes Nothing Nothing rootId blankNodes
--  in  VersionedDocument {rootId = rootId, versionedNodes = versionedNodes}

--addNodesToDocument : Array (RawNode tData) -> Maybe Int -> Maybe Int -> Int ->
--                        Array (VersionedNode tData) -> Array (VersionedNode tData)
--addNodesToDocument rawNodes maybeParentId maybeNodeIndex nodeId nodes =
--  case (Array.get nodeId rawNodes) of
--    Just (RawLeaf data) ->
--      let newNode = VersionedNode {parentId = maybeParentId, index = maybeNodeIndex, versionId = 0, documentNode = DataNode data}
--      in Array.set nodeId newNode nodes
--    Just (RawNode {childIds}) ->
--      let addChild (index, id) nds = addNodesToDocument rawNodes (Just nodeId) (Just index) id nds
--          indexedChildIds = List.indexedMap (,) childIds
--          childNodes = List.foldl addChild nodes indexedChildIds
--          newNode = VersionedNode {
--            parentId = maybeParentId,
--            index = maybeNodeIndex,
--            versionId = 0,
--            documentNode = InternalNode {
----              childIndices = Array.fromList childIds
--               childIndices = childIds
--           }
--          }
--      in  Array.set nodeId newNode childNodes
--    _ -> nodes -- TODO: Report this branch as error
