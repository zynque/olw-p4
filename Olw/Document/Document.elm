module Olw.Document.Document
    exposing
        ( Document
        , Node
        , childrenOf
        , getNode
        , parentOf
        , pathFromRootTo
        )

import Array.Hamt as Array exposing (Array)
import Olw.Document.Data exposing (..)
import Olw.Document.ElmCoreExt exposing (maybeFlatten)
import String exposing (join)


-- An (Open Language Web) document is a persistent data structure
-- representing a tree with data at each of its nodes (internal or leaf)
-- It is backed by a Relaxed Radix B tree, or Hash Array Mapped Trie
-- for efficient updates
--


type alias Document tData =
    { rootId : Int -- index of root node in nodes array
    , nodes : Array (Node tData) -- indexed by node id
    }


type alias Node tData =
    { version : Int, data : tData, childIds : List Int, parentId : Maybe Int }


getNode : Int -> Document tData -> Maybe (Node tData)
getNode nodeId doc =
    Array.get nodeId doc.nodes


childrenOf : Int -> Document tData -> List Int
childrenOf nodeId doc =
    let
        maybeNode =
            doc |> getNode nodeId

        children =
            Maybe.map (\node -> node.childIds) maybeNode
    in
    Maybe.withDefault [] children


parentOf : Int -> Document tData -> Maybe Int
parentOf nodeId doc =
    getNode nodeId doc
        |> Maybe.andThen (\n -> n.parentId)


pathFromRootTo : Int -> Document tData -> List Int
pathFromRootTo nodeId wdoc =
    pathToRootFrom nodeId wdoc |> List.reverse


pathToRootFrom : Int -> Document tData -> List Int
pathToRootFrom nodeId wdoc =
    case parentOf nodeId wdoc of
        Nothing ->
            [ nodeId ]

        Just parentId ->
            nodeId :: pathToRootFrom parentId wdoc
