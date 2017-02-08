module Olw.Document.Document exposing (
    Document(..),
    VersionedNode(..),
    Node(..),
    getNode,
    childrenOf
  )

import Array.Hamt as Array exposing (Array)
import String exposing (join)
import Olw.Document.Data exposing (..)

type Document tData = Document {
  rootId: Int,
  versionedNodes: Array (VersionedNode tData) -- indexed by node id
}

type VersionedNode tData = VersionedNode {
  version: Int,
  node: Node tData
}

type Node tData = Node {data: tData, childIds: List Int}

getNode : Int -> Document tData -> Maybe (VersionedNode tData)
getNode nodeId vDoc =
  let (Document {rootId, versionedNodes}) = vDoc
  in  Array.get nodeId versionedNodes

childrenOf : Int -> Document tData -> List Int
childrenOf nodeId doc =
  let maybeNode = doc |> getNode nodeId
      childrenOfNode (Node {childIds}) = childIds
      childrenOfVersionedNode (VersionedNode {version, node}) =
        childrenOfNode node
      children = Maybe.map childrenOfVersionedNode maybeNode  
  in  Maybe.withDefault [] children
