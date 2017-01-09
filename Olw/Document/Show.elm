module Olw.Document.Show exposing (
    showDocumentResult,
    showDocument
  )

import Array exposing (Array)
import String exposing (join)
import Olw.Document.Data exposing (..)
import Olw.Document.Document exposing (..)

--showResult : Result String 

showDocumentResult : Result String (Document DocumentData) -> List String
showDocumentResult docResult = case docResult of
  Ok doc -> showDocument doc
  Err err -> [err]

showDocument : Document DocumentData -> List String
showDocument (Document {rootId, versionedNodes}) =
  let indexedChildren = Array.toIndexedList versionedNodes
      displayChild (i, c) = "n:" ++ (toString i) ++ " " ++ (showNode c)
      displayedChildren = List.map displayChild indexedChildren
  in ("root:" ++ (toString rootId)) :: displayedChildren

showNode : VersionedNode DocumentData -> String
showNode versionedNode =
  let (VersionedNode {version, node}) = versionedNode
      content = case node of
--        (InternalNode {childIndices}) -> "(" ++ join "," (Array.toList (Array.map toString childIndices)) ++ ")"
        (Node {childIds}) -> "(" ++ join "," (List.map toString childIds) ++ ")"
        (Leaf data) -> (showDocumentData data)
  in  "v:" ++ (toString version) ++ " - " ++ content

showDocumentData : DocumentData -> String
showDocumentData data =
  case data of
    StringData s -> "\"" ++ s ++ "\""
    IntData i    -> toString i
    _            -> "?"
