module Olw.Document.Show exposing (
    showWorkingDocument,
    showDocument,
    showResult
  )

import Array exposing (Array)
import String exposing (join)
import Olw.Document.Data exposing (..)
import Olw.Document.Document exposing (..)
import Olw.Document.WorkingDocument exposing (..)

showResult : (t -> List String) -> Result String t -> List String
showResult show res =
  case res of
    Ok x -> show x
    Err err -> [err]

showWorkingDocument : WorkingDocument DocumentData -> List String
showWorkingDocument wdoc =
  let (WorkingDocument {document, parentIds}) = wdoc
      shownDoc = showDocument document
      shownParentIds = "parents:" ++ (toString (Array.toIndexedList parentIds))
  in  "Working Document" :: shownParentIds :: shownDoc

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
