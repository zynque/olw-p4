module Olw.DocumentVNav exposing(updateNodeData, offsetDocBy, arrayInsertBefore, insertNode)

import Array exposing (Array)
import String exposing (join)
import Olw.Document as Document exposing (..)
import Olw.Detached as Detached exposing (..)
import Olw.DocumentV as DocumentV exposing (..)
import Olw.DocumentVBuilder as DocumentVBuilder exposing (..)

getVersionedNode : Int -> VersionedDocument tData -> Maybe (VersionedNode tData)
getVersionedNode nodeId vDoc =
  let (VersionedDocument {rootId, versionedNodes}) = vDoc
  in  Array.get nodeId versionedNodes

parentOf : Int -> VersionedDocument tData -> Maybe (Int)
parentOf nodeId doc =
  let maybeNode = doc |> getVersionedNode nodeId
      parentOfVersionedNode (VersionedNode {parentId, versionId, documentNode}) = parentId
  in  case maybeNode of
      Just node -> parentOfVersionedNode node
      Nothing -> Nothing

childrenOf : Int -> VersionedDocument tData -> List Int
childrenOf nodeId doc =
  let maybeNode = doc |> getVersionedNode nodeId
      childrenOfDocumentNode docNode = case docNode of
--        InternalNode {childIndices} -> Array.toList childIndices
        InternalNode {childIndices} -> childIndices
        _ -> []
      childrenOfNodeVersion (VersionedNode {parentId, versionId, documentNode}) =
        childrenOfDocumentNode documentNode
      children = Maybe.map childrenOfNodeVersion maybeNode  
  in  Maybe.withDefault [] children

offsetNodeBy : Int -> VersionedNode tData -> VersionedNode tData
offsetNodeBy offset node =
  let (VersionedNode {parentId, versionId, documentNode}) = node
      newDocNode = case documentNode of
        InternalNode {childIndices} ->
          --InternalNode {childIndices = Array.map (\n -> n + offset) childIndices}
          InternalNode {childIndices = List.map (\n -> n + offset) childIndices}
        other -> other
  in  VersionedNode {
        parentId = Maybe.map (\n -> n + offset) parentId,
        versionId = versionId,
        documentNode = newDocNode
      }

offsetNodesBy : Int -> Array (VersionedNode tData) -> List (VersionedNode tData)
offsetNodesBy offset nodes =
  let nodesList = Array.toList nodes
  in List.map (offsetNodeBy offset) nodesList

offsetDocBy : Int -> VersionedDocument tData -> VersionedDocument tData
offsetDocBy offset doc =
  let (VersionedDocument {rootId, versionedNodes}) = doc
      offsetVersionedNodes = Array.map (offsetNodeBy offset) versionedNodes
  in  VersionedDocument {rootId = rootId + offset, versionedNodes = offsetVersionedNodes}

insertNode : DetachedNode tData -> Int -> Int ->
             VersionedDocument tData -> Result String (VersionedDocument tData)
insertNode detachedNode parentId index doc =
  let (VersionedDocument {rootId, versionedNodes}) = doc
      docToMerge = DocumentVBuilder.buildDocument detachedNode
      originalNodesLength = Array.length versionedNodes
      docAfterOffset = offsetDocBy (originalNodesLength) docToMerge
      (newNodeId, newNodes) =
        let (VersionedDocument {rootId, versionedNodes}) = docAfterOffset
        in (rootId, versionedNodes)
      docWithAddedNodes = VersionedDocument {
        rootId = rootId,
        versionedNodes = Array.append versionedNodes newNodes
      }
      res = setNodesParent newNodeId parentId docWithAddedNodes
  in  case res of
        Ok doc -> addChildToParentInDoc parentId newNodeId index doc
        err -> err

addChildToParentInDoc : Int -> Int -> Int ->
                        VersionedDocument tData -> Result String (VersionedDocument tData)
addChildToParentInDoc parentId childId index doc =
  let update docNode =
    case docNode of
    InternalNode {childIndices} -> InternalNode {
      childIndices = listInsertBefore index childId childIndices
    }
    other -> other
  in  transformNodeContent parentId update doc

setNodesParent : Int -> Int ->
                 VersionedDocument tData -> Result String (VersionedDocument tData)
setNodesParent nodeId parentId doc =
  transformNode nodeId (setParent parentId) doc

setParent : Int -> VersionedNode tData -> VersionedNode tData
setParent parentId newNode =
  let (VersionedNode {parentId, versionId, documentNode}) = newNode
  in  VersionedNode {parentId = parentId, versionId = versionId, documentNode = documentNode}   

arrayInsertBefore : Int -> t -> Array t -> Array t
arrayInsertBefore index item arr =
  let left  = Array.slice 0 (index) arr
      right = Array.slice index (Array.length arr) arr
  in  Array.append (Array.push item left) right 

listInsertBefore : Int -> t -> List t -> List t
listInsertBefore index item lst =
  let left  = List.take (index) lst
      right = List.drop (index) lst
  in  List.append left (item :: right) 

updateNodeData : Int -> tData ->
             VersionedDocument tData -> Result String (VersionedDocument tData)
updateNodeData nodeId newData oldDoc =
  updateNodeContent nodeId (DataNode newData) oldDoc

updateNodeContent : Int -> DocumentNode tData ->
                    VersionedDocument tData -> Result String (VersionedDocument tData)
updateNodeContent nodeId newData oldDoc =
  transformNodeContent nodeId (\n -> newData) oldDoc

transformNodeContent : Int -> (DocumentNode tData -> DocumentNode tData) ->
                    VersionedDocument tData -> Result String (VersionedDocument tData)
transformNodeContent nodeId updateContent oldDoc =
  let updateVersionedNode (VersionedNode {parentId, versionId, documentNode}) =
        VersionedNode {
          parentId = parentId,
          versionId = versionId + 1,
          documentNode = updateContent(documentNode)
        }
  in transformNode nodeId updateVersionedNode oldDoc

transformNode : Int -> (VersionedNode tData -> VersionedNode tData) ->
                VersionedDocument tData -> Result String (VersionedDocument tData)
transformNode nodeId update oldDoc =
  let (VersionedDocument {rootId, versionedNodes}) = oldDoc
      maybeOldVersionedNode = Array.get nodeId versionedNodes
      maybeNewVersionedNode = Maybe.map update maybeOldVersionedNode
  in case maybeNewVersionedNode of
      Just newVersionedNode ->       
        let newVersionedNodes = Array.set nodeId newVersionedNode versionedNodes
            newDoc = VersionedDocument {rootId = rootId, versionedNodes = newVersionedNodes}
        in  Ok newDoc
      _ -> Err ("DocumentV.updateNode: Node id: " ++ (toString nodeId) ++ " does not exist in document")

--moveNode : Int -> Int -> Int ->
--           VersionedDocument tData -> Result String (VersionedDocument tData)
--moveNode nodeId newParentId newIndex oldDoc =

--deleteNode : Int -> VersionedDocument tData -> Result String (VersionedDocument tData)
