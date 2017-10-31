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
  let (rootId, nodes) = addDetachedNode detached Array.empty
      emptyParentIds = Array.repeat (Array.length nodes) Nothing
      parentIds = getParentNodeIds Nothing rootId nodes emptyParentIds
  in  {
    rootId = rootId,
    nodes = nodes,
    parentIds = parentIds
  }


-- addDetachedNode
-- recursive helper method takes doc of nodes added so far
-- and returns it with the new detached node & its decendants added
-- the rootId is the assigned id of the detached node
-- parentIds is left empty to be filled in later
addDetachedNode : DetachedNode tData -> Array (Node tData) -> (Int, Array (Node tData))
addDetachedNode detachedNode nodes =
  let {data, detachedChildren} = detachedNode
      (DetachedChildren children) = detachedChildren

      addChild detached (ids, nodes) =
        let (newRootId, newNodes) = addDetachedNode detached nodes
        in (newRootId :: ids, newNodes)

      (childIdsInReverse, decendantNodes) = List.foldl addChild ([], nodes) children

      childIds = List.reverse childIdsInReverse
      newNode = {version = 0, data = data, childIds = childIds}
      newNodes = Array.push newNode decendantNodes
      newRootId = Array.length decendantNodes
  in  (newRootId, newNodes)


getParentNodeIds : Maybe Int -> Int -> Array (Node tData) -> Array (Maybe Int) -> Array (Maybe Int)
getParentNodeIds parentId nodeId nodes parentIds =
  let updatedParents = Array.set nodeId parentId parentIds
      maybeNode = Array.get nodeId nodes
      maybeChildIds = Maybe.map (\node -> node.childIds) maybeNode
      childIds = Maybe.withDefault [] maybeChildIds
      getChildsParent childId parentIds = getParentNodeIds (Just nodeId) childId nodes parentIds
  in  List.foldl getChildsParent updatedParents childIds
