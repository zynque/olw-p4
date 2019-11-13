module Olw.Document.Edit
    exposing
        ( cutNode
        , insertNode
        , pasteNode
        , updateNodeData
        )

import Array.Hamt as Array exposing (Array)
import Olw.Document.Build as Build exposing (..)
import Olw.Document.Data exposing (..)
import Olw.Document.Detached as Detached exposing (..)
import Olw.Document.Document exposing (..)
import Olw.Document.ElmCoreExt exposing (..)
import String exposing (join)


insertNode :
    DetachedNode tData
    -> Int
    -> Int
    -> Document tData
    -> Result String (Document tData)
insertNode detachedNode parentId index document =
    let
        docToMerge =
            Build.buildDocument detachedNode

        originalNodesLength =
            Array.length document.nodes

        docAfterOffset =
            offsetDocBy originalNodesLength docToMerge

        newNodeId =
            docAfterOffset.rootId

        docWithAddedNodes =
            { rootId = document.rootId
            , nodes = Array.append document.nodes docAfterOffset.nodes
            }

        docResult =
            setNodesParent newNodeId parentId docWithAddedNodes
    in
    docResult |> Result.andThen (\doc -> addChildToParentInDoc parentId newNodeId index doc)



-- cutNode
-- removes a node with given id by updating its parent to no longer include
-- it as a child
-- the node remains in the documents node array until cleanup is performed


cutNode : Int -> Document tData -> Result String (Document tData)
cutNode nodeId document =
    let
        maybeParentId =
            getNode nodeId document
                |> Maybe.andThen (\n -> n.parentId)

        updateParentContent node =
            { version = node.version + 1
            , data = node.data
            , childIds = List.filter (\i -> i /= nodeId) node.childIds
            , parentId = node.parentId
            }
    in
    case maybeParentId of
        Just parentId ->
            document
                |> transformNodeContent parentId updateParentContent

        Nothing ->
            Err ("Edit.cutNode: could not lookup parent of node with id: " ++ toString nodeId)



-- paste a node that was previously cut in a new location in the document
-- will result in error if the node already exists in document


pasteNode :
    Int
    -> Int
    -> Int
    -> Document tData
    -> Result String (Document tData)
pasteNode nodeId parentId index document =
    setNodesParent nodeId parentId document
        |> Result.andThen (\d -> addChildToParentInDoc parentId nodeId index d)


offsetNodeBy : Int -> Node tData -> Node tData
offsetNodeBy offset node =
    let
        newChildIds =
            List.map (\n -> n + offset) node.childIds
    in
    { version = node.version
    , data = node.data
    , childIds = newChildIds
    , parentId = Maybe.map (\id -> id + offset) node.parentId -- todo: test this
    }


offsetNodesBy : Int -> Array (Node tData) -> List (Node tData)
offsetNodesBy offset nodes =
    let
        nodesList =
            Array.toList nodes
    in
    List.map (offsetNodeBy offset) nodesList


offsetDocBy : Int -> Document tData -> Document tData
offsetDocBy offset doc =
    let
        offsetVersionedNodes =
            Array.map (offsetNodeBy offset) doc.nodes
    in
    { rootId = doc.rootId + offset
    , nodes = offsetVersionedNodes
    }


offsetParentsBy : Int -> Array (Maybe Int) -> Array (Maybe Int)
offsetParentsBy offset parents =
    let
        applyOffset maybeParent =
            case maybeParent of
                Just parentId ->
                    Just (parentId + offset)

                Nothing ->
                    Nothing
    in
    Array.map applyOffset parents


addChildToParentInDoc :
    Int
    -> Int
    -> Int
    -> Document tData
    -> Result String (Document tData)
addChildToParentInDoc parentId childId index doc =
    let
        update node =
            { version = node.version + 1
            , data = node.data
            , childIds = listInsertBefore index childId node.childIds
            , parentId = node.parentId
            }
    in
    transformNodeContent parentId update doc


setNodesParent : Int -> Int -> Document tData -> Result String (Document tData)
setNodesParent nodeId newParentId document =
    let
        update n =
            { n | parentId = Just newParentId }
    in
    transformNode nodeId update document


updateNodeData :
    Int
    -> tData
    -> Document tData
    -> Result String (Document tData)
updateNodeData nodeId newData oldDoc =
    let
        transform node =
            { version = node.version + 1, data = newData, childIds = node.childIds, parentId = node.parentId }
    in
    transformNodeContent nodeId transform oldDoc


updateNodeChildIds :
    Int
    -> List Int
    -> Document tData
    -> Result String (Document tData)
updateNodeChildIds nodeId newChildIds oldDoc =
    let
        transform node =
            { version = node.version + 1, data = node.data, childIds = newChildIds, parentId = node.parentId }
    in
    transformNodeContent nodeId transform oldDoc



-- Redundant - remove


transformNodeContent :
    Int
    -> (Node tData -> Node tData)
    -> Document tData
    -> Result String (Document tData)
transformNodeContent nodeId updateContent oldDoc =
    transformNode nodeId updateContent oldDoc


transformNode :
    Int
    -> (Node tData -> Node tData)
    -> Document tData
    -> Result String (Document tData)
transformNode nodeId update document =
    let
        maybeOldVersionedNode =
            Array.get nodeId document.nodes

        maybeNewVersionedNode =
            Maybe.map update maybeOldVersionedNode
    in
    case maybeNewVersionedNode of
        Just newVersionedNode ->
            let
                newVersionedNodes =
                    Array.set nodeId newVersionedNode document.nodes

                newDoc =
                    { rootId = document.rootId
                    , nodes = newVersionedNodes
                    }
            in
            Ok newDoc

        _ ->
            Err ("DocumentV.updateNode: Node id: " ++ toString nodeId ++ " does not exist in document")
