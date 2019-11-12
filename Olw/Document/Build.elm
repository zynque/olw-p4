module Olw.Document.Build
    exposing
        ( beginDocument
        , buildDocument
        )

import Array.Hamt as Array exposing (Array)
import Olw.Document.Data exposing (..)
import Olw.Document.Detached as Detached exposing (..)
import Olw.Document.Document as Document exposing (..)
import String exposing (join)


beginDocument : tData -> Document tData
beginDocument data =
    { emptyDocument |
      nodes = Array.fromList
            [ { version = 0
              , data = data
              , childIds = []
              , parentId = Nothing
              }
            ]
    }


buildDocument : DetachedNode tData -> Document tData
buildDocument detached =
    let
        detachedWithIds = assignIds detached

        ( rootId, nodes ) =
            addDetachedNode Nothing detachedWithIds Array.empty

        emptyParentIds =
            Array.repeat (Array.length nodes) Nothing

        parentIds =
            getParentNodeIds Nothing rootId nodes emptyParentIds
    in
    { rootId = rootId
    , nodes = nodes
    , parentIds = parentIds
    }


emptyDocument =
    { rootId = 0, nodes = Array.fromList [], parentIds = Array.empty }


type alias DetachedNodeWithId tData =
    { id: Int, data : tData, children: DetachedChildrenWithIds tData}


type DetachedChildrenWithIds tData =
    DetachedChildrenWithIds (List (DetachedNodeWithId tData))


assignIds : DetachedNode tData -> DetachedNodeWithId tData
assignIds node = 
    let
        ( _, detachedNodeWithId ) = assignIdsFrom 0 node
    in
        detachedNodeWithId


assignIdsFrom : Int -> DetachedNode tData -> (Int, DetachedNodeWithId tData)
assignIdsFrom startingId node =
    let
        { data, detachedChildren } = node

        (DetachedChildren children) = detachedChildren

        accumulateAssignedIds child (start, childrenWithIds) =
            let
                (nextId, childWithAssignedId) = assignIdsFrom start child
            in
                (nextId, childWithAssignedId :: childrenWithIds)

        (nextId, reversedChildrenWithIds) = List.foldl accumulateAssignedIds (startingId, []) children

        childrenWithIds = List.reverse reversedChildrenWithIds
    in
        (nextId + 1, { id = nextId, data = data, children = DetachedChildrenWithIds childrenWithIds })


-- addDetachedNode
-- recursive helper method takes doc of nodes added so far
-- and returns it with the new detached node & its decendants added
-- the rootId is the assigned id of the detached node
-- parentIds is left empty to be filled in later


addDetachedNode : Maybe Int -> DetachedNodeWithId tData -> Array (Node tData) -> ( Int, Array (Node tData) )
addDetachedNode parentId detachedNode nodes =
    let
        { id, data, children } = detachedNode

        (DetachedChildrenWithIds children2) = children

        addChild detached ( ids, nodes ) =
            let
                ( newRootId, newNodes ) =
                    addDetachedNode (Just id) detached nodes
            in
            ( newRootId :: ids, newNodes )

        ( childIdsInReverse, decendantNodes ) =
            List.foldl addChild ( [], nodes ) children2

        childIds =
            List.reverse childIdsInReverse

        newNode =
            { version = 0, data = data, childIds = childIds, parentId = parentId }

        newNodes =
            Array.push newNode decendantNodes

        newRootId =
            Array.length decendantNodes
    in
    ( newRootId, newNodes )


getParentNodeIds : Maybe Int -> Int -> Array (Node tData) -> Array (Maybe Int) -> Array (Maybe Int)
getParentNodeIds parentId nodeId nodes parentIds =
    let
        updatedParents =
            Array.set nodeId parentId parentIds

        maybeNode =
            Array.get nodeId nodes

        maybeChildIds =
            Maybe.map (\node -> node.childIds) maybeNode

        childIds =
            Maybe.withDefault [] maybeChildIds

        getChildsParent childId parentIds =
            getParentNodeIds (Just nodeId) childId nodes parentIds
    in
    List.foldl getChildsParent updatedParents childIds
