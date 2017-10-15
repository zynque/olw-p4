module Olw.Document.Show exposing (
    showWorkingDocument,
    showDocument,
    showResult,
    showNodeData
  )

import Array.Hamt as Array exposing (Array)
import String exposing (join)
import Olw.Document.Data exposing (..)
import Olw.Document.Document exposing (..)
import Olw.Document.WorkingDocument exposing (..)

showResult : (t -> List String) -> Result String t -> List String
showResult show res =
  case res of
    Ok x -> show x
    Err err -> [err]

showWorkingDocument : WorkingDocument NodeData -> List String
showWorkingDocument wdoc =
  let (WorkingDocument {document, parentIds}) = wdoc
      shownDoc = showDocument document
      shownParentIds = "parents:" ++ (toString (Array.toIndexedList parentIds))
  in  shownParentIds :: shownDoc

showDocument : Document NodeData -> List String
showDocument document =
  let indexedChildren = Array.toIndexedList document.nodes
      displayChild (i, c) = "n:" ++ (toString i) ++ " " ++ (showNode c)
      displayedChildren = List.map displayChild indexedChildren
  in ("root:" ++ (toString document.rootId)) :: displayedChildren

showNode : Node NodeData -> String
showNode node =
  let content =
         "d:(" ++ (showNodeData node.data) ++ ") c:(" ++ join "," (List.map toString node.childIds) ++ ")"
  in  "v:" ++ (toString node.version) ++ " - " ++ content

showNodeData : NodeData -> String
showNodeData data =
  case data of
    StringData s -> "\"" ++ s ++ "\""
    IntData i    -> toString i
    _            -> "?"
