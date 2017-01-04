module Olw.DocumentV exposing(
  VersionedDocument(..),
  VersionedNode(..),
  showDocument,
  showDocumentResult)

import Array exposing (Array)
import String exposing (join)
import Olw.Document as Document exposing (..)
import Olw.Detached as Detached exposing (..)

-- Versioned Document with necessary navigation info (parent id, relative index)

type VersionedDocument tData = VersionedDocument {
  rootId: Int,
  versionedNodes: Array (VersionedNode tData) -- indexed by node id
}

type VersionedNode tData = VersionedNode {
  parentId: Maybe Int,
  versionId: Int, -- incremented for each new version of a node
  documentNode: DocumentNode tData
}

showDocumentResult : Result String (VersionedDocument DocumentData) -> String
showDocumentResult docResult = case docResult of
  Ok doc -> showDocument doc
  Err err -> err

showDocument : VersionedDocument DocumentData -> String
showDocument (VersionedDocument {rootId, versionedNodes}) =
  let indexedChildren = Array.toIndexedList versionedNodes
      displayChild (i, c) = (toString i) ++ ":" ++ (showDocumentNode c)
      displayedChildren = List.map displayChild indexedChildren
  in "root:" ++ (toString rootId) ++ " | " ++ (join " | " displayedChildren)


showDocumentNode : VersionedNode DocumentData -> String
showDocumentNode node =
  let (VersionedNode {versionId, documentNode}) = node
      content = case documentNode of
        (InternalNode {childIndices}) -> "(" ++ join "," (Array.toList (Array.map toString childIndices)) ++ ")"
        (DataNode data) -> (showDocumentData data)
  in  (toString versionId) ++ ":" ++ content

showDocumentData : DocumentData -> String
showDocumentData data =
  case data of
    StringData s -> "\"" ++ s ++ "\""
    IntData i    -> toString i
    _            -> "?"
