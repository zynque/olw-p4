module Olw.Document.Edit exposing (
    insertNode,
    updateNodeData,
    cutNode,
    pasteNode
  )

import Array exposing (Array)
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
      newNode = case node of
        Node {childIds} ->
          --InternalNode {childIndices = Array.map (\n -> n + offset) childIndices}
          Node {childIds = List.map (\n -> n + offset) childIds}
        other -> other
  in  VersionedNode {
        version = version,
        node = newNode
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
  let wd = setNodesParent nodeId parentId workingDocument
  in  addChildToParentInDoc parentId nodeId index wd

addChildToParentInDoc : Int -> Int -> Int ->
                        WorkingDocument tData -> Result String (WorkingDocument tData)
addChildToParentInDoc parentId childId index doc =
  let update docNode =
    case docNode of
    Node {childIds} -> Node {
      childIds = listInsertBefore index childId childIds
    }
    other -> other
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
  updateNode nodeId (Leaf newData) oldDoc

updateNode : Int -> Node tData ->
                    WorkingDocument tData -> Result String (WorkingDocument tData)
updateNode nodeId newData oldDoc =
  transformNodeContent nodeId (\n -> newData) oldDoc

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
      updateParentContent parentData =
        case parentData of
          Node {childIds} ->
            Node {childIds = List.filter (\i -> i /= nodeId) childIds}
          other -> other
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
