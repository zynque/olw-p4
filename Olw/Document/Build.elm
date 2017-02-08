module Olw.Document.Build exposing (
    buildDocument,
    buildWorkingDocument,
    beginWorkingDocument
  )

import Array exposing (Array)
import String exposing (join)
import Olw.Document.Data exposing (..)
import Olw.Document.Detached as Detached exposing (..)
import Olw.Document.Document as Document exposing (..)
import Olw.Document.WorkingDocument as WorkingDocument exposing (..)

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
      (DetachedNode {data, children}) = detachedNode

      addChild detached (ids, doc) =
        let newDoc = addDetachedNode detached doc
            (Document {rootId, versionedNodes}) = newDoc
        in (rootId :: ids, newDoc)

      (childIdsRev, docWithChildren) = List.foldl addChild ([], document) children

      result =
        let (Document {rootId, versionedNodes}) = docWithChildren
            newRootId = Array.length versionedNodes
            childIds = List.reverse childIdsRev
            newNode = VersionedNode {version = 0, node = Node {data = data, childIds = childIds}}
            newNodes = Array.push newNode versionedNodes
        in  Document {rootId = newRootId, versionedNodes = newNodes}
  in  result


buildWorkingDocument : Document tData -> WorkingDocument tData
buildWorkingDocument document =
  let (Document {rootId, versionedNodes}) = document
      emptyParentIds = Array.repeat (Array.length versionedNodes) Nothing
      parentIds = setParentNodeIds Nothing rootId document emptyParentIds
  in  WorkingDocument {document = document, parentIds = parentIds}

setParentNodeIds : Maybe Int -> Int -> Document tData -> Array (Maybe Int) -> Array (Maybe Int)
setParentNodeIds parentId nodeId document parentIds =
  let updatedParents = Array.set nodeId parentId parentIds 
      childIds = document |> Document.childrenOf nodeId
      setChildsParent childId parentIds = setParentNodeIds (Just nodeId) childId document parentIds
  in  List.foldl setChildsParent updatedParents childIds

beginWorkingDocument : tData -> WorkingDocument tData
beginWorkingDocument data =
  let document = Document {
    rootId = 0,
    versionedNodes = Array.fromList [
      VersionedNode {version = 0, node = Node {data = data, childIds = []}}
    ]
  }
  in buildWorkingDocument document
