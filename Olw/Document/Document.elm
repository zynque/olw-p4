module Olw.Document.Document exposing (
    Document,
    Node,
    getNode,
    childrenOf,
    parentOf,
    pathFromRootTo
  )

import Array.Hamt as Array exposing (Array)
import String exposing (join)
import Olw.Document.Data exposing (..)
import Olw.Document.ElmCoreExt exposing (maybeFlatten)


type alias Document tData = {
  rootId: Int,

  -- indexed by node id
  nodes: Array (Node tData),

  -- indexed by node id, gives that node's parent id
  parentIds: Array (Maybe Int)
  
  -- indexed by node id, gives the 0-based index of that node within its parent's list of children
  -- positions: Array (Maybe Int)
}


type alias Node tData = {version: Int, data: tData, childIds: List Int}


getNode : Int -> Document tData -> Maybe (Node tData)
getNode nodeId doc = Array.get nodeId doc.nodes


childrenOf : Int -> Document tData -> List Int
childrenOf nodeId doc =
  let maybeNode = doc |> getNode nodeId
      children = Maybe.map (\node -> node.childIds) maybeNode  
  in  Maybe.withDefault [] children


parentOf : Int -> Document tData -> Maybe (Int)
parentOf nodeId doc = maybeFlatten (Array.get nodeId doc.parentIds)


pathFromRootTo : Int -> Document tData -> List Int
pathFromRootTo nodeId wdoc = pathToRootFrom nodeId wdoc |> List.reverse


pathToRootFrom : Int -> Document tData -> List Int
pathToRootFrom nodeId wdoc =
  case (parentOf nodeId wdoc) of
    Nothing -> [nodeId]
    Just parentId -> nodeId :: (pathToRootFrom parentId wdoc)
