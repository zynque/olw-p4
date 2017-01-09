module Olw.Document.Build exposing (
    buildDocument,
    buildWorkingDocument
  )

import Array exposing (Array)
import String exposing (join)
import Olw.Document.Data exposing (..)
import Olw.Document.Detached as Detached exposing (..)
import Olw.Document.Document exposing (..)
import Olw.Document.WorkingDocument exposing (..)

emptyDocument = Document {rootId = 0, versionedNodes = Array.fromList []}

buildDocument : DetachedNode tData -> Document tData
buildDocument detached =
  addDetachedNode detached emptyDocument

-- addDetachedNode
-- recursive helper method takes doc of nodes added so far
-- and returns it with the new detached node & its children added
-- the rootId is the assigned id of the detached node

addDetachedNode : DetachedNode tData -> Document tData -> Document tData
addDetachedNode detachedNode document =
  let (Document {rootId, versionedNodes}) = document
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
          (childIdsRev, docWithChildren) = List.foldl addChild ([], document) children
          (Document {rootId, versionedNodes}) = docWithChildren
          newRootId = Array.length versionedNodes
          childIds = List.reverse childIdsRev
          newNode = VersionedNode {version = 0, node = Node {childIds = childIds}}
          newNodes = Array.push newNode versionedNodes
      in  Document {rootId = newRootId, versionedNodes = newNodes}

buildWorkingDocument : Document tData -> WorkingDocument tData
buildWorkingDocument document =
  let (Document {rootId, versionedNodes}) = document
      emptyParentIds = Array.repeat (Array.length versionedNodes) Nothing
      parentIds = setParentNodeIds Nothing rootId document emptyParentIds
  in  WorkingDocument {document = document, parentIds = parentIds}

setParentNodeIds : Maybe Int -> Int -> Document tData -> Array (Maybe Int) -> Array (Maybe Int)
setParentNodeIds parentId nodeId document parentIds =
  let updatedParents = Array.set nodeId parentId parentIds 
      childIds = document |> childrenOf nodeId
      setChildsParent childId parentIds = setParentNodeIds (Just nodeId) childId document parentIds
  in  List.foldl setChildsParent updatedParents childIds
