module Olw.Document.Document exposing (
    Document,
    Node,
    getNode,
    childrenOf
  )

import Array.Hamt as Array exposing (Array)
import String exposing (join)
import Olw.Document.Data exposing (..)

type alias Document tData = {
  rootId: Int,
  nodes: Array (Node tData), -- indexed by node id
  parentIds: Array (Maybe Int)
}

type alias Node tData = {version: Int, data: tData, childIds: List Int}

getNode : Int -> Document tData -> Maybe (Node tData)
getNode nodeId doc =
  Array.get nodeId doc.nodes

childrenOf : Int -> Document tData -> List Int
childrenOf nodeId doc =
  let maybeNode = doc |> getNode nodeId
      childrenOfVersionedNode node = node.childIds
      children = Maybe.map childrenOfVersionedNode maybeNode  
  in  Maybe.withDefault [] children
