module Olw.Document exposing (
    DocumentIdentifier(..),
    InternalReference(..),
    ExternalReference(..),
    DocumentData(..),
    Document(..),
    DocumentNode(..),
    childrenOf,
    showDocument
  )

import Array exposing (Array)
import String exposing (join)


type DocumentIdentifier = DocumentIdentifier String
type InternalReference = InternalReference Int
type ExternalReference = ExternalReference { document: DocumentIdentifier, version: Int, nodeId: Int }


type DocumentData =
  StringData String |
  IntData Int |
  ExternalReferenceNode ExternalReference |
  InternalReferenceNode InternalReference


type Document tData = Document {
  rootId: Int,
  nodes: Array (DocumentNode tData)
}

-- todo: convert childIndices back to an array once elm's array bugs can be fixed
type DocumentNode tData = InternalNode {childIndices: List Int} | DataNode tData


childrenOf : Int -> Document tData -> List Int
childrenOf nodeId doc =
  let (Document {rootId, nodes}) = doc
      node = Array.get nodeId nodes
      childIds = case node of
        --Just (InternalNode {childIndices}) -> Array.toList childIndices
        Just (InternalNode {childIndices}) -> childIndices
        _ -> []
  in childIds


showDocument : Document String -> String
showDocument (Document {rootId, nodes}) =
  let indexedChildren = Array.toIndexedList nodes
      displayChild (i, c) = (toString i) ++ ":" ++ (showDocumentNode c)
      displayedChildren = List.map displayChild indexedChildren
  in toString rootId ++ "!" ++ (join "|" displayedChildren)


showDocumentNode : DocumentNode String -> String
showDocumentNode node = case node of
--  (InternalNode {childIndices}) -> "(" ++ join "," (Array.toList (Array.map toString childIndices)) ++ ")"
  (InternalNode {childIndices}) -> "(" ++ join "," (List.map toString childIndices) ++ ")"
  (DataNode data) -> "\"" ++ data ++ "\""
