module Olw.Document.Document exposing (
    Document(..),
    Node,
    getNode,
    childrenOf
  )

import Array.Hamt as Array exposing (Array)
import String exposing (join)
import Olw.Document.Data exposing (..)

type Document tData = Document {
  rootId: Int,
  nodes: Array (Node tData), -- indexed by node id
  parentIds: Array (Maybe Int)
}

type alias Node tData = {version: Int, data: tData, childIds: List Int}

getNode : Int -> Document tData -> Maybe (Node tData)
getNode nodeId vDoc =
  let (Document {rootId, nodes}) = vDoc
  in  Array.get nodeId nodes

childrenOf : Int -> Document tData -> List Int
childrenOf nodeId doc =
  let maybeNode = doc |> getNode nodeId
      childrenOfVersionedNode node = node.childIds
      children = Maybe.map childrenOfVersionedNode maybeNode  
  in  Maybe.withDefault [] children
