module Olw.Document.Show exposing (
    showDocument,
    showResult,
    showNodeData
  )

import Array.Hamt as Array exposing (Array)
import String exposing (join)
import Olw.Document.Data exposing (..)
import Olw.Document.Document exposing (..)

showResult : (t -> List String) -> Result String t -> List String
showResult show res =
  case res of
    Ok x -> show x
    Err err -> [err]


showDocument : Document NodeData -> List String
showDocument document =
  let indexedChildren = Array.toIndexedList document.nodes
      displayChild (i, c) = "n:" ++ (toString i) ++ " " ++ (showNode c)
      displayedChildren = List.map displayChild indexedChildren
      displayedParentIds = " parents:" ++ (toString (Array.toIndexedList document.parentIds))
  in ("root:" ++ (toString document.rootId) ++ displayedParentIds) :: displayedChildren


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
    FloatData f  -> toString f
    _            -> "?"
