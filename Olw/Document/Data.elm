module Olw.Document.Data exposing (
    NodeData(..),
    ExternalNodeReference
  )

type NodeData =
  StringData String |
  IntData Int |
  InternalNodeRef Int |
  ExternalNodeRef ExternalNodeReference

type alias ExternalNodeReference = {
  documentUrl: String,
  documentVersionId: Int,
  nodeId: Int
}
