module Olw.Document.Edit exposing (
    updateNodeData,
    insertNode,
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


insertNode : DetachedNode tData -> Int -> Int ->
             WorkingDocument tData -> Result String (WorkingDocument tData)
insertNode detachedNode parentId index workingDocument =
  let (WorkingDocument {document, parentIds}) = workingDocument
      (Document {rootId, nodes}) = document
      docToMerge = Build.buildDocument detachedNode
      workingDocToMerge = Build.buildWorkingDocumentFromDocument docToMerge
      parentIdsToMerge =
        let (WorkingDocument {parentIds}) = workingDocToMerge
        in  parentIds
      originalNodesLength = Array.length nodes
      docAfterOffset = offsetDocBy originalNodesLength docToMerge
      parentIdsAfterOffset = offsetParentsBy originalNodesLength parentIdsToMerge
      (newNodeId, newNodes) =
        let (Document {rootId, nodes}) = docAfterOffset
        in  (rootId, nodes)
      docWithAddedNodes = Document {
        rootId = rootId,
        nodes = Array.append nodes newNodes,
        parentIds = Array.empty
      }
      workingDocWithAddedNodes = WorkingDocument {
        document = docWithAddedNodes,
        parentIds = Array.append parentIds parentIdsAfterOffset
      }
      wd = setNodesParent newNodeId parentId workingDocWithAddedNodes
  in  addChildToParentInDoc parentId newNodeId index wd


-- cutNode
-- removes a node with given id by updating its parent to no longer include
-- it as a child
-- the node remains in the documents node array until cleanup is performed
cutNode : Int -> WorkingDocument tData -> Result String (WorkingDocument tData)
cutNode nodeId workingDocument =
  let (WorkingDocument {document, parentIds}) = workingDocument
      maybeParentId = maybeFlatten (Array.get nodeId parentIds)
      updateParentContent node =
        {
          version = node.version + 1,
          data = node.data,
          childIds = List.filter (\i -> i /= nodeId) node.childIds
        }
      clearParent = Array.set nodeId Nothing
  in  case maybeParentId of
        Just parentId ->
          workingDocument
            |> transformParentIds clearParent
            |> transformNodeContent parentId updateParentContent
        Nothing -> Err ("Edit.cutNode: could not lookup parent of node with id: " ++ (toString nodeId))


-- paste a node that was previously cut in a new location in the document
-- will result in error if the node already exists in document
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


offsetNodeBy : Int -> Node tData -> Node tData
offsetNodeBy offset node =
  let newChildIds = List.map (\n -> n + offset) node.childIds
  in  {
        version = node.version,
        data = node.data,
        childIds = newChildIds
      }


offsetNodesBy : Int -> Array (Node tData) -> List (Node tData)
offsetNodesBy offset nodes =
  let nodesList = Array.toList nodes
  in List.map (offsetNodeBy offset) nodesList


offsetDocBy : Int -> Document tData -> Document tData
offsetDocBy offset doc =
  let (Document {rootId, nodes}) = doc
      offsetVersionedNodes = Array.map (offsetNodeBy offset) nodes
  in  Document {
    rootId = rootId + offset,
    nodes = offsetVersionedNodes,
    parentIds = Array.empty
  }


offsetParentsBy : Int -> Array (Maybe Int) -> Array (Maybe Int)
offsetParentsBy offset parents =
  let applyOffset maybeParent = case maybeParent of
    Just parentId -> Just (parentId + offset)
    Nothing -> Nothing
  in  Array.map applyOffset parents


addChildToParentInDoc : Int -> Int -> Int ->
                        WorkingDocument tData -> Result String (WorkingDocument tData)
addChildToParentInDoc parentId childId index doc =
  let update node =
        {
          version = node.version + 1,
          data = node.data,
          childIds = listInsertBefore index childId node.childIds
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
  let transform node = {version = node.version + 1, data = newData, childIds = node.childIds}
  in  transformNodeContent nodeId transform oldDoc


updateNodeChildIds : Int -> List Int ->
                    WorkingDocument tData -> Result String (WorkingDocument tData)
updateNodeChildIds nodeId newChildIds oldDoc =
  let transform node = {version = node.version + 1, data = node.data, childIds = newChildIds}
  in  transformNodeContent nodeId transform oldDoc


transformNodeContent : Int -> (Node tData -> Node tData) ->
                    WorkingDocument tData -> Result String (WorkingDocument tData)
transformNodeContent nodeId updateContent oldDoc =
  transformNode nodeId updateContent oldDoc


transformNode : Int -> (Node tData -> Node tData) ->
                WorkingDocument tData -> Result String (WorkingDocument tData)
transformNode nodeId update workingDocument =
  let (WorkingDocument {document, parentIds}) = workingDocument
      (Document {rootId, nodes}) = document
      maybeOldVersionedNode = Array.get nodeId nodes
      maybeNewVersionedNode = Maybe.map update maybeOldVersionedNode
  in case maybeNewVersionedNode of
      Just newVersionedNode ->       
        let newVersionedNodes = Array.set nodeId newVersionedNode nodes
            newDoc = Document {
              rootId = rootId,
              nodes = newVersionedNodes,
              parentIds = Array.empty
            }
            newWorkingDoc = WorkingDocument {document = newDoc, parentIds = parentIds}
        in  Ok newWorkingDoc
      _ -> Err ("DocumentV.updateNode: Node id: " ++ (toString nodeId) ++ " does not exist in document")


transformParentIds : (Array (Maybe Int) -> Array (Maybe Int)) ->
                     WorkingDocument tData -> WorkingDocument tData
transformParentIds transform workingDocument =
  let (WorkingDocument {document, parentIds}) = workingDocument
      updatedParentIds = transform parentIds
  in  WorkingDocument {document = document, parentIds = updatedParentIds}
