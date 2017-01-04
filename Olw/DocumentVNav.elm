module Olw.DocumentVNav exposing(updateNodeData)

import Array exposing (Array)
import String exposing (join)
import Olw.Document as Document exposing (..)
import Olw.Detached as Detached exposing (..)
import Olw.DocumentV as DocumentV exposing (..)

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
        InternalNode {childIndices} -> Array.toList childIndices
        _ -> []
      childrenOfNodeVersion (VersionedNode {parentId, versionId, documentNode}) =
        childrenOfDocumentNode documentNode
      children = Maybe.map childrenOfNodeVersion maybeNode  
  in  Maybe.withDefault [] children

--insertNode : detachedNode tData -> Int (parent) -> Int (index) ->
--             VersionedDocument tData -> Result String (VersionedDocument tData)

updateNodeData : Int -> tData ->
             VersionedDocument tData -> Result String (VersionedDocument tData)
updateNodeData nodeId newData oldDoc =
  let (VersionedDocument {rootId, versionedNodes}) = oldDoc
      updateVersionedNode (VersionedNode {parentId, versionId, documentNode}) =
        VersionedNode {parentId = parentId, versionId = versionId + 1, documentNode = DataNode newData}
      maybeOldVersionedNode = Array.get nodeId versionedNodes
      maybeNewVersionedNode = Maybe.map updateVersionedNode maybeOldVersionedNode
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
