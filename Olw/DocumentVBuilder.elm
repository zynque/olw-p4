module Olw.DocumentVBuilder exposing(buildDocument, buildRawDocument, showRawDocument)

import Array exposing (Array)
import String exposing (join)
import Olw.Document as Document exposing (..)
import Olw.Detached as Detached exposing (..)
import Olw.DocumentV as DocumentV exposing (..)


-- RawDocument
-- Document tree without additional navigation info (parent id)

type RawDocument tData = RawDocument {
  rootId: Int,
  nodes: Array (RawNode tData) -- indexed by node id
}

type RawNode tData = RawNode {childIds: List Int} | RawLeaf tData

buildRawDocument : DetachedNode tData -> RawDocument tData
buildRawDocument detached =
  addDetachedNodeToRawDoc detached (RawDocument {rootId = 0, nodes = Array.fromList []})

-- addDetachedNodeToRawDoc
-- recursive helper method takes doc of nodes added so far
-- and returns it with the new detached node & its children added
-- the rootId is the assigned id of the detached node

addDetachedNodeToRawDoc : DetachedNode tData -> RawDocument tData -> RawDocument tData
addDetachedNodeToRawDoc detachedNode rawDoc =
  let (RawDocument {rootId, nodes}) = rawDoc
  in case detachedNode of
    DetachedDataNode data ->
      let rootId = Array.length nodes
          newNodes = Array.push (RawLeaf data) nodes
      in RawDocument {rootId = rootId, nodes = newNodes}
    DetachedInternalNode children ->
      let addChild detached (ids, rawDoc) =
            let newRawDoc = addDetachedNodeToRawDoc detached rawDoc
                (RawDocument {rootId, nodes}) = newRawDoc
            in (rootId :: ids, newRawDoc)
          (childIdsRev, rawDocWithChildren) = List.foldl addChild ([], rawDoc) children
          (RawDocument {rootId, nodes}) = rawDocWithChildren
          newRootId = Array.length nodes
          newNode = RawNode {childIds = List.reverse childIdsRev}
          newNodes = Array.push newNode nodes
      in  RawDocument {rootId = newRootId, nodes = newNodes}

showRawDocument : RawDocument tData -> String
showRawDocument rawDoc =
  let (RawDocument {rootId, nodes}) = rawDoc
  in  "root: " ++ (toString rootId) ++ " | " ++ showRemainingRawDocNodes nodes rootId

showRemainingRawDocNodes nodes nodeId =
  case (Array.get nodeId nodes) of
  Just (RawNode {childIds}) ->
    let thisNode = (toString nodeId) ++ ": " ++ (toString childIds) ++ " | "
        shownChildren = List.map (showRemainingRawDocNodes nodes) childIds
    in  thisNode ++ (String.concat shownChildren)    
  _ -> ""

blankNode = VersionedNode {
        parentId = Nothing,
        versionId = 0,
        index = Nothing,
--        documentNode = InternalNode {childIndices = Array.fromList []}
        documentNode = InternalNode {childIndices = []}
      }

buildDocument : DetachedNode tData -> VersionedDocument tData
buildDocument detached =
  let rawDoc = buildRawDocument detached
      (RawDocument {rootId, nodes}) = rawDoc
      numNodes = Array.length nodes
      blankNodes = Array.repeat numNodes blankNode
      versionedNodes = addNodesToDocument nodes Nothing Nothing rootId blankNodes
  in  VersionedDocument {rootId = rootId, versionedNodes = versionedNodes}

addNodesToDocument : Array (RawNode tData) -> Maybe Int -> Maybe Int -> Int ->
                        Array (VersionedNode tData) -> Array (VersionedNode tData)
addNodesToDocument rawNodes maybeParentId maybeNodeIndex nodeId nodes =
  case (Array.get nodeId rawNodes) of
    Just (RawLeaf data) ->
      let newNode = VersionedNode {parentId = maybeParentId, index = maybeNodeIndex, versionId = 0, documentNode = DataNode data}
      in Array.set nodeId newNode nodes
    Just (RawNode {childIds}) ->
      let addChild (index, id) nds = addNodesToDocument rawNodes (Just nodeId) (Just index) id nds
          indexedChildIds = List.indexedMap (,) childIds
          childNodes = List.foldl addChild nodes indexedChildIds
          newNode = VersionedNode {
            parentId = maybeParentId,
            index = maybeNodeIndex,
            versionId = 0,
            documentNode = InternalNode {
--              childIndices = Array.fromList childIds
               childIndices = childIds
           }
          }
      in  Array.set nodeId newNode childNodes
    _ -> nodes -- TODO: Report this branch as error
