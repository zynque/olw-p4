module Olw.Document.Build exposing (
    buildDocument,
    beginDocument
  )

import Array.Hamt as Array exposing (Array)
import String exposing (join)
import Olw.Document.Data exposing (..)
import Olw.Document.Detached as Detached exposing (..)
import Olw.Document.Document as Document exposing (..)


beginDocument : tData -> Document tData
beginDocument data = {
    rootId = 0,
    nodes = Array.fromList [
      {version = 0, data = data, childIds = []}
    ],
    parentIds = Array.empty
  }


emptyDocument = {rootId = 0, nodes = Array.fromList [], parentIds = Array.empty}


buildDocument : DetachedNode tData -> Document tData
buildDocument detached =
  let newDoc = addDetachedNode detached emptyDocument
      emptyParentIds = Array.repeat (Array.length newDoc.nodes) Nothing
      parentIds = setParentNodeIds Nothing newDoc.rootId newDoc emptyParentIds
  in  {
    rootId = newDoc.rootId,
    nodes = newDoc.nodes,
    parentIds = parentIds
  }


-- addDetachedNode
-- recursive helper method takes doc of nodes added so far
-- and returns it with the new detached node & its children added
-- the rootId is the assigned id of the detached node
-- parentIds is left empty to be filled in later
addDetachedNode : DetachedNode tData -> Document tData -> Document tData
addDetachedNode detachedNode document =
  let {data, children} = detachedNode
      (DetachedChildren childList) = children

      addChild detached (ids, doc) =
        let newDoc = addDetachedNode detached doc
        in (newDoc.rootId :: ids, newDoc)

      (childIdsRev, docWithChildren) = List.foldl addChild ([], document) childList

      newDoc =
        let newRootId = Array.length docWithChildren.nodes
            childIds = List.reverse childIdsRev
            newNode = {version = 0, data = data, childIds = childIds}
            newNodes = Array.push newNode docWithChildren.nodes
        in  {
          rootId = newRootId,
          nodes = newNodes,
          parentIds = Array.empty
        }
  in  newDoc


setParentNodeIds : Maybe Int -> Int -> Document tData -> Array (Maybe Int) -> Array (Maybe Int)
setParentNodeIds parentId nodeId document parentIds =
  let updatedParents = Array.set nodeId parentId parentIds 
      childIds = document |> Document.childrenOf nodeId
      setChildsParent childId parentIds = setParentNodeIds (Just nodeId) childId document parentIds
  in  List.foldl setChildsParent updatedParents childIds
