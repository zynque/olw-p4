module Olw.Document.Show
    exposing
        ( showDocument
        , showNodeData
        , showResult
        )

import Array.Hamt as Array exposing (Array)
import Olw.Document.Data exposing (..)
import Olw.Document.Document exposing (..)
import String exposing (join)


showResult : (t -> List String) -> Result String t -> List String
showResult show res =
    case res of
        Ok x ->
            show x

        Err err ->
            [ err ]


showDocument : Document NodeData -> List String
showDocument document =
    let
        indexedChildren =
            Array.toIndexedList document.nodes

        displayChild ( i, c ) =
            "n:" ++ toString i ++ " " ++ showNode c

        displayedChildren =
            List.map displayChild indexedChildren
    in
    ("root:" ++ toString document.rootId) :: displayedChildren


showNode : Node NodeData -> String
showNode node =
    let
        content =
            "d:(" ++ showNodeData node.data ++ ") " ++ "p:(" ++ showParentId node.parentId ++ ")" ++ "c:(" ++ join "," (List.map toString node.childIds) ++ ")"
    in
    "v:" ++ toString node.version ++ " - " ++ content


showParentId : Maybe Int -> String
showParentId id =
    case id of
        Just i ->
            toString i

        Nothing ->
            ""


showNodeData : NodeData -> String
showNodeData data =
    case data of
        StringData s ->
            "\"" ++ s ++ "\""

        IntData i ->
            toString i

        FloatData f ->
            toString f

        _ ->
            "?"
