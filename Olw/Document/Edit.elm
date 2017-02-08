module Olw.Document.Edit exposing (
    insertNode,
    updateNodeData,
    cutNode,
    pasteNode
  )

import Array.Hamt as Array exposing (Array)
import String exposing (join)
import Olw.Document.Data exposing (..)
import Olw.Document.Detached as Detached exposing (..)
import Olw.Document.Document exposing (..)
import Olw.Document.WorkingDocument exposing (..)
import Olw.Document.ElmCoreExt exposing (..)
import Olw.Document.Build as Build exposing (..)

offsetNodeBy : Int -> VersionedNode tData -> VersionedNode tData
offsetNodeBy offset versionedNode =
  let (VersionedNode {version, node}) = versionedNode
      (Node {data, childIds}) = node
      newChildIds = List.map (\n -> n + offset) childIds
  in  VersionedNode {
        version = version,
        node = Node {data = data, childIds = newChildIds}
      }

offsetNodesBy : Int -> Array (VersionedNode tData) -> List (VersionedNode tData)
offsetNodesBy offset nodes =
  let nodesList = Array.toList nodes
  in List.map (offsetNodeBy offset) nodesList

offsetDocBy : Int -> Document tData -> Document tData
offsetDocBy offset doc =
  let (Document {rootId, versionedNodes}) = doc
      offsetVersionedNodes = Array.map (offsetNodeBy offset) versionedNodes
  in  Document {rootId = rootId + offset, versionedNodes = offsetVersionedNodes}

offsetParentsBy : Int -> Array (Maybe Int) -> Array (Maybe Int)
offsetParentsBy offset parents =
  let applyOffset maybeParent = case maybeParent of
    Just parentId -> Just (parentId + offset)
    Nothing -> Nothing
  in  Array.map applyOffset parents

insertNode : DetachedNode tData -> Int -> Int ->
             WorkingDocument tData -> Result String (WorkingDocument tData)
insertNode detachedNode parentId index workingDocument =
  let (WorkingDocument {document, parentIds}) = workingDocument
      (Document {rootId, versionedNodes}) = document
      docToMerge = Build.buildDocument detachedNode
      workingDocToMerge = Build.buildWorkingDocument docToMerge
      parentIdsToMerge =
        let (WorkingDocument {parentIds}) = workingDocToMerge
        in  parentIds
      originalNodesLength = Array.length versionedNodes
      docAfterOffset = offsetDocBy originalNodesLength docToMerge
      parentIdsAfterOffset = offsetParentsBy originalNodesLength parentIdsToMerge
      (newNodeId, newNodes) =
        let (Document {rootId, versionedNodes}) = docAfterOffset
        in  (rootId, versionedNodes)
      docWithAddedNodes = Document {
        rootId = rootId,
        versionedNodes = Array.append versionedNodes newNodes
      }
      workingDocWithAddedNodes = WorkingDocument {
        document = docWithAddedNodes,
        parentIds = Array.append parentIds parentIdsAfterOffset
      }
      wd = setNodesParent newNodeId parentId workingDocWithAddedNodes
  in  addChildToParentInDoc parentId newNodeId index wd

pasteNode : Int -> Int -> Int ->
            WorkingDocument tData -> Result String (WorkingDocument tData)
pasteNode nodeId parentId index workingDocument =
  let (WorkingDocument {parentIds}) = workingDocument
      currentParent = maybeFlatten (Array.get nodeId parentIds)
  in  case currentParent of
        Just id -> Err ("Edit.pasteNode: Node with id: " ++ (toString nodeId) ++ " is already attached to document")
        _ ->
          let wd = setNodesParent nodeId parentId workingDocument
          in  addChildToParentInDoc parentId nodeId index wd

addChildToParentInDoc : Int -> Int -> Int ->
                        WorkingDocument tData -> Result String (WorkingDocument tData)
addChildToParentInDoc parentId childId index doc =
  let update (Node {data, childIds}) =
        Node {
          data = data,
          childIds = listInsertBefore index childId childIds
        }
  in  transformNodeContent parentId update doc

setNodesParent : Int -> Int -> WorkingDocument tData -> WorkingDocument tData
setNodesParent nodeId newParentId workingDocument =
  let (WorkingDocument {parentIds, document}) = workingDocument
  in  WorkingDocument {
    parentIds = Array.set nodeId (Just newParentId) parentIds,
    document = document
  }

updateNodeData : Int -> tData ->
             WorkingDocument tData -> Result String (WorkingDocument tData)
updateNodeData nodeId newData oldDoc =
  let transform (Node {data, childIds}) = Node {data = newData, childIds = childIds}
  in  transformNodeContent nodeId transform oldDoc

updateNodeChildIds : Int -> List Int ->
                    WorkingDocument tData -> Result String (WorkingDocument tData)
updateNodeChildIds nodeId newChildIds oldDoc =
  let transform (Node {data, childIds}) = Node {data = data, childIds = newChildIds}
  in  transformNodeContent nodeId transform oldDoc

transformNodeContent : Int -> (Node tData -> Node tData) ->
                    WorkingDocument tData -> Result String (WorkingDocument tData)
transformNodeContent nodeId updateContent oldDoc =
  let updateVersionedNode (VersionedNode {version, node}) =
        VersionedNode {
          version = version + 1,
          node = updateContent(node)
        }
  in transformNode nodeId updateVersionedNode oldDoc

transformNode : Int -> (VersionedNode tData -> VersionedNode tData) ->
                WorkingDocument tData -> Result String (WorkingDocument tData)
transformNode nodeId update workingDocument =
  let (WorkingDocument {document, parentIds}) = workingDocument
      (Document {rootId, versionedNodes}) = document
      maybeOldVersionedNode = Array.get nodeId versionedNodes
      maybeNewVersionedNode = Maybe.map update maybeOldVersionedNode
  in case maybeNewVersionedNode of
      Just newVersionedNode ->       
        let newVersionedNodes = Array.set nodeId newVersionedNode versionedNodes
            newDoc = Document {rootId = rootId, versionedNodes = newVersionedNodes}
            newWorkingDoc = WorkingDocument {document = newDoc, parentIds = parentIds}
        in  Ok newWorkingDoc
      _ -> Err ("DocumentV.updateNode: Node id: " ++ (toString nodeId) ++ " does not exist in document")

-- cutNode
-- removes a node with given id by updating its parent to no longer include
-- it as a child
-- the node remains in the documents node array until cleanup is performed
cutNode : Int -> WorkingDocument tData -> Result String (WorkingDocument tData)
cutNode nodeId workingDocument =
  let (WorkingDocument {document, parentIds}) = workingDocument
      maybeParentId = maybeFlatten (Array.get nodeId parentIds)
      updateParentContent (Node {data, childIds}) =
        Node {
          data = data,
          childIds = List.filter (\i -> i /= nodeId) childIds
        }
      clearParent = Array.set nodeId Nothing
  in  case maybeParentId of
        Just parentId ->
          workingDocument
            |> transformParentIds clearParent
            |> transformNodeContent parentId updateParentContent
        Nothing -> Err ("Edit.cutNode: could not lookup parent of node with id: " ++ (toString nodeId))

transformParentIds : (Array (Maybe Int) -> Array (Maybe Int)) ->
                     WorkingDocument tData -> WorkingDocument tData
transformParentIds transform workingDocument =
  let (WorkingDocument {document, parentIds}) = workingDocument
      updatedParentIds = transform parentIds
  in  WorkingDocument {document = document, parentIds = updatedParentIds}
