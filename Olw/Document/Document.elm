module Olw.Document.Document exposing (
    Document(..),
    VersionedNode(..),
    Node(..)
  )

import Array exposing (Array)
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

type Node tData = Node {childIds: List Int} | Leaf tData
