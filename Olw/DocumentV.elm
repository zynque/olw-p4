module Olw.DocumentV exposing(..)

import Array exposing (Array)
import String exposing (join)
import Olw.Document as Document exposing (..)
import Olw.Detached as Detached exposing (..)

-- Versioned Document with necessary navigation info (parent id, relative index)

type VersionedDocument tData = VersionedDocument {
  rootId: Int,
  versionedNodes: Array (VersionedNode tData) -- indexed by node id
}

type VersionedNode tData = VersionedNode {
  parentId: Maybe Int,
  versionId: Int, -- incremented for each new version of a node
  documentNode: DocumentNode tData
}

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

updateNode : Int -> DocumentNode tData ->
             VersionedDocument tData -> Result String (VersionedDocument tData)
updateNode nodeId newDocNode oldDoc =
  let (VersionedDocument {rootId, versionedNodes}) = oldDoc
      updateVersionedNode (VersionedNode {parentId, versionId, documentNode}) =
        VersionedNode {parentId = parentId, versionId = versionId + 1, documentNode = newDocNode}
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

-- RawDocument
-- Document tree without additional navigation info (parent id)

type RawDocument tData = RawDocument {
  rootId: Int,
  nodes: Array (RawNode tData) -- indexed by node id
}
type RawNode tData = RawNode {childIds: List Int} | RawLeaf tData

buildRawDocument : DetachedNode tData -> RawDocument tData
buildRawDocument detached =
  buildRawDocument' detached (RawDocument {rootId = 0, nodes = Array.fromList []})

-- buildRawDocument'
-- recursive helper method takes doc of nodes added so far
-- and returns it with the new detached node & its children added
-- the rootId is the assigned id of the detached node

buildRawDocument' : DetachedNode tData -> RawDocument tData -> RawDocument tData
buildRawDocument' detachedNode rawDoc =
  let (RawDocument {rootId, nodes}) = rawDoc
  in case detachedNode of
    DetachedDataNode data ->
      let rootId = Array.length nodes
          newNodes = Array.push (RawLeaf data) nodes
      in RawDocument {rootId = rootId, nodes = newNodes}
    DetachedInternalNode children ->
      let addChild detached (ids, rawDoc) =
            let newSpine = buildRawDocument' detached rawDoc
                (RawDocument {rootId, nodes}) = newSpine
            in (rootId :: ids, newSpine)
          (childIds, rawDocWithChildren) = List.foldl addChild ([], rawDoc) children
          (RawDocument {rootId, nodes}) = rawDocWithChildren
          newRootId = Array.length nodes
          newNode = RawNode {childIds = childIds}
          newNodes = Array.push newNode nodes
      in  RawDocument {rootId = newRootId, nodes = newNodes}

showRawDocument : RawDocument tData -> String
showRawDocument rawDoc =
  let (RawDocument {rootId, nodes}) = rawDoc
  in  "root: " ++ (toString rootId) ++ " | " ++ showRawDocument' nodes rootId

showRawDocument' nodes nodeId =
  case (Array.get nodeId nodes) of
  Just (RawNode {childIds}) ->
    let thisNode = (toString nodeId) ++ ": " ++ (toString childIds) ++ " | "
        shownChildren = List.map (showRawDocument' nodes) childIds
    in  thisNode ++ (String.concat shownChildren)    
  _ -> ""

blankNode = VersionedNode {
        parentId = Nothing,
        versionId = 0,
        documentNode = InternalNode {childIndices = Array.fromList []}
      }

buildDocument : DetachedNode tData -> VersionedDocument tData
buildDocument detached =
  let rawDoc = buildRawDocument detached
      (RawDocument {rootId, nodes}) = rawDoc
      numNodes = Array.length nodes
      blankNodes = Array.repeat numNodes blankNode
      versionedNodes = buildDocument' nodes Nothing rootId blankNodes
  in  VersionedDocument {rootId = rootId, versionedNodes = versionedNodes}

buildDocument' : Array (RawNode tData) -> Maybe Int -> Int ->
                        Array (VersionedNode tData) -> Array (VersionedNode tData)
buildDocument' rawNodes parentId nodeId nodes =
  case (Array.get nodeId rawNodes) of
    Just (RawLeaf data) ->
      let newNode = VersionedNode {parentId = parentId, versionId = 0, documentNode = DataNode data}
      in Array.set nodeId newNode nodes
    Just (RawNode {childIds}) ->
      let addChild id nds = buildDocument' rawNodes (Just nodeId) id nds
          childNodes = List.foldl addChild nodes childIds
          newNode = VersionedNode {
            parentId = parentId,
            versionId = 0,
            documentNode = InternalNode {
              childIndices = Array.fromList childIds
            }
          }
      in  Array.set nodeId newNode childNodes
    _ -> nodes -- TODO: Report this branch as error

showDocument : VersionedDocument DocumentData -> String
showDocument (VersionedDocument {rootId, versionedNodes}) =
  let indexedChildren = Array.toIndexedList versionedNodes
      displayChild (i, c) = (toString i) ++ ":" ++ (showDocumentNode c)
      displayedChildren = List.map displayChild indexedChildren
  in "root:" ++ (toString rootId) ++ " | " ++ (join " | " displayedChildren)


showDocumentNode : VersionedNode DocumentData -> String
showDocumentNode node =
  let (VersionedNode {versionId, documentNode}) = node
      content = case documentNode of
        (InternalNode {childIndices}) -> "(" ++ join "," (Array.toList (Array.map toString childIndices)) ++ ")"
        (DataNode data) -> (showDocumentData data)
  in  (toString versionId) ++ ":" ++ content

showDocumentData : DocumentData -> String
showDocumentData data =
  case data of
    StringNode s -> "\"" ++ s ++ "\""
    IntNode i    -> toString i
    _            -> "?"
