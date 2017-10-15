module Olw.Document.Build exposing (
    buildDocument,
    buildWorkingDocumentFromDocument,
    buildWorkingDocument,
    beginWorkingDocument
  )

import Array.Hamt as Array exposing (Array)
import String exposing (join)
import Olw.Document.Data exposing (..)
import Olw.Document.Detached as Detached exposing (..)
import Olw.Document.Document as Document exposing (..)
import Olw.Document.WorkingDocument as WorkingDocument exposing (..)


buildWorkingDocument : DetachedNode tData -> WorkingDocument tData
buildWorkingDocument = buildDocument >> buildWorkingDocumentFromDocument


beginWorkingDocument : tData -> WorkingDocument tData
beginWorkingDocument data =
  let document = Document {
    rootId = 0,
    nodes = Array.fromList [
      {version = 0, data = data, childIds = []}
    ],
    parentIds = Array.empty
  }
  in buildWorkingDocumentFromDocument document


emptyDocument = Document {rootId = 0, nodes = Array.fromList [], parentIds = Array.empty}


buildDocument : DetachedNode tData -> Document tData
buildDocument detached =
  addDetachedNode detached emptyDocument


-- addDetachedNode
-- recursive helper method takes doc of nodes added so far
-- and returns it with the new detached node & its children added
-- the rootId is the assigned id of the detached node
addDetachedNode : DetachedNode tData -> Document tData -> Document tData
addDetachedNode detachedNode document =
  let (Document {rootId, nodes}) = document
      {data, children} = detachedNode
      (DetachedChildren childList) = children

      addChild detached (ids, doc) =
        let newDoc = addDetachedNode detached doc
            (Document {rootId, nodes}) = newDoc
        in (rootId :: ids, newDoc)

      (childIdsRev, docWithChildren) = List.foldl addChild ([], document) childList

      result =
        let (Document {rootId, nodes}) = docWithChildren
            newRootId = Array.length nodes
            childIds = List.reverse childIdsRev
            newNode = {version = 0, data = data, childIds = childIds}
            newNodes = Array.push newNode nodes
        in  Document {rootId = newRootId, nodes = newNodes, parentIds = Array.empty}
  in  result


buildWorkingDocumentFromDocument : Document tData -> WorkingDocument tData
buildWorkingDocumentFromDocument document =
  let (Document {rootId, nodes}) = document
      emptyParentIds = Array.repeat (Array.length nodes) Nothing
      parentIds = setParentNodeIds Nothing rootId document emptyParentIds
  in  WorkingDocument {document = document, parentIds = parentIds}


setParentNodeIds : Maybe Int -> Int -> Document tData -> Array (Maybe Int) -> Array (Maybe Int)
setParentNodeIds parentId nodeId document parentIds =
  let updatedParents = Array.set nodeId parentId parentIds 
      childIds = document |> Document.childrenOf nodeId
      setChildsParent childId parentIds = setParentNodeIds (Just nodeId) childId document parentIds
  in  List.foldl setChildsParent updatedParents childIds
