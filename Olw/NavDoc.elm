module Olw.NavDoc exposing (
    NavDoc(..),
    fromDocument, parentOf, childrenOf, pathToRoot, update
  )

import List
import Array exposing (Array)
import String exposing (join)
import Dict exposing (Dict)
import Olw.Document as Document exposing (..)


type NavDoc tData = NavDoc {
  doc: Document tData,
  parents: Dict Int Int,
  relativeIndices: Dict Int Int
}


fromDocument : Document tData -> NavDoc tData
fromDocument doc =
  let (Document { rootId, nodes }) = doc
      childIds = Document.childrenOf rootId doc
      parents = addParents doc rootId Dict.empty
      relativeIndices = addIndicesRelativeToParents doc rootId Dict.empty
  in NavDoc { doc = doc, parents = parents, relativeIndices = relativeIndices }


parentOf : Int -> NavDoc tData -> Maybe Int
parentOf nodeId (NavDoc { doc, parents }) =
  Dict.get nodeId parents


childrenOf : Int -> NavDoc tData -> List Int
childrenOf nodeId (NavDoc { doc, parents }) =
  Document.childrenOf nodeId doc


-- returns path from specified node id up to root of tree
-- (head of list is that node's id, end of list is doc root)
pathToRoot : Int -> NavDoc tData -> List Int
pathToRoot nodeId navDoc =
  nodeId :: updPathToRoot nodeId navDoc []


updPathToRoot : Int -> NavDoc tData -> List Int -> List Int
updPathToRoot nodeId navDoc path =
  let parentIdOpt = parentOf nodeId navDoc
  in case parentIdOpt of
    Just parentId ->
      let p = updPathToRoot parentId navDoc path
      in parentId :: p
    _ -> path


addParents : Document tData -> Int -> Dict Int Int -> Dict Int Int
addParents doc nodeId dict =
  let (Document { rootId, nodes }) = doc
      childIds = Document.childrenOf nodeId doc
      dictWithThisNode = addParent nodeId childIds dict
      completedDict = List.foldl (\i d -> addParents doc i d) dictWithThisNode childIds 
  in Dict.union dict completedDict


addParent : Int -> List Int -> Dict Int Int -> Dict Int Int
addParent parentId childIds dict =
  let nodeParents = List.map (\i -> (i, parentId)) childIds
      parentDict = Dict.fromList nodeParents
  in Dict.union dict parentDict


addIndicesRelativeToParents : Document tData -> Int -> Dict Int Int -> Dict Int Int
addIndicesRelativeToParents doc nodeId dict =
  let (Document { rootId, nodes }) = doc
      childIds = Document.childrenOf nodeId doc
      dictWithThisNode = addIndicesRelativeToParent nodeId childIds dict
      completedDict = List.foldl (\i d -> addIndicesRelativeToParents doc i d) dictWithThisNode childIds 
  in Dict.union dict completedDict


addIndicesRelativeToParent : Int -> List Int -> Dict Int Int -> Dict Int Int
addIndicesRelativeToParent parentId childIds dict =
  let nodeIndices = List.indexedMap (\index nodeId -> (nodeId, index)) childIds
      indicesDict = Dict.fromList nodeIndices
  in Dict.union dict indicesDict


update : Int -> DocumentNode tData -> NavDoc tData -> Result String (NavDoc tData, List Int, List Int)
update replacedNodeId newNode navDoc =
  let (NavDoc { doc, parents, relativeIndices }) = navDoc
      (Document { rootId, nodes }) = doc
      path = pathToRoot replacedNodeId navDoc
      newNodeId = Array.length nodes
      nodesWithNewNode = Array.push newNode nodes
      result = replaceNodeOnPathInDoc path [newNodeId] newNodeId newNode relativeIndices nodesWithNewNode
  in case result of
    Ok ((newRootId :: newPath), createdNodes) ->
      let d = Document { rootId = newRootId, nodes = createdNodes }
          p = updateParents path (newRootId :: newPath) d parents
          r = updateRelativeIndices path (newRootId :: newPath) d relativeIndices
      in Ok (NavDoc { doc = d, parents = p, relativeIndices = r }, path, newRootId :: newPath)
    Err e -> Err e
    Ok (newPath, ns) -> Err ("update was unable to determine root id because new path was not generated properly: " ++ (toString newPath))


updateParents : List Int -> List Int -> Document tData ->
                Dict Int Int -> Dict Int Int
updateParents oldPathToRoot newPathToRoot doc parents =
  parents
    |> rmOldParents oldPathToRoot 
    |> addNewParents newPathToRoot doc


rmOldParents : List Int -> Dict Int Int -> Dict Int Int
rmOldParents oldPathToRoot parents =
  List.foldl Dict.remove parents oldPathToRoot 


addNewParents : List Int -> Document tData ->
                Dict Int Int -> Dict Int Int
addNewParents newPathToRoot doc parents =
  let addChildren newParentId dict =
        let children = Document.childrenOf newParentId doc
        in List.foldl (\c d -> Dict.insert c newParentId d) dict children
  in List.foldl addChildren parents newPathToRoot


updateRelativeIndices : List Int -> List Int -> Document tData ->
                        Dict Int Int -> Dict Int Int
updateRelativeIndices oldPathToRoot newPathToRoot doc relativeIndices =
  let zip = List.map2 (,)
      pairs = zip oldPathToRoot newPathToRoot
  in List.foldl updateRelativeIndex relativeIndices pairs


updateRelativeIndex : (Int, Int) -> Dict Int Int -> Dict Int Int
updateRelativeIndex (oldNID, newNID) d =
  let relInd = Dict.get oldNID d
  in case relInd of
    Just i ->
      d |> Dict.remove oldNID
        |> Dict.insert newNID i
    Nothing -> d -- TODO: report error in this case


replaceNodeOnPathInDoc : List Int -> List Int -> Int -> DocumentNode tData ->
                         Dict Int Int ->
                         Array (DocumentNode tData) ->
                         Result String (List Int, Array (DocumentNode tData))
replaceNodeOnPathInDoc oldPathToRoot newPathFromRoot newNodeId newNode relativeIndices nodes =
  case oldPathToRoot of
    replacedNodeId :: parentId :: remainingPath ->
      let relativeIndex = Dict.get replacedNodeId relativeIndices
      in case relativeIndex of
        Just i ->
          let resultOfInsert = insertNodeUnderParent parentId replacedNodeId i newNodeId newNode nodes
          in case resultOfInsert of
            Ok (newParentId, newParentNode, newNodes) ->
              replaceNodeOnPathInDoc (parentId :: remainingPath) (newParentId :: newPathFromRoot) newParentId newParentNode relativeIndices newNodes
            Err msg -> Err msg
        Nothing -> Err ("replaceNodeOnPathInDoc expected node in relative index table with id: " ++ toString replacedNodeId)
    _ -> Ok (newPathFromRoot, nodes)


insertNodeUnderParent : Int -> Int -> Int -> Int -> DocumentNode tData ->
                        Array (DocumentNode tData) ->
                        Result String (Int, DocumentNode tData, Array (DocumentNode tData))
insertNodeUnderParent parentId replacedNodeId relativeIndex newNodeId newNode nodes =
  let parentNode = Array.get parentId nodes
      newNavDoc = case parentNode of
        Just (InternalNode { childIndices }) ->
          let newChildIds = listSetAt relativeIndex newNodeId childIndices
              --newChildIds = Array.set relativeIndex newNodeId childIndices
              newParentNode = InternalNode { childIndices = newChildIds }
              newParentId = Array.length nodes
              nodesWithNewParent = Array.push newParentNode nodes
          in Ok (newParentId, newParentNode, nodesWithNewParent)
        Nothing -> Err ("insertNodeUnderParent failed to find parent node with id: " ++ toString parentId)
        _ -> Err ("insertNodeUnderParent expected parent to be InternalNode with id: " ++ toString parentId)
  in newNavDoc

listSetAt : Int -> t -> List t -> List t
listSetAt index item lst =
  let left  = List.take (index) lst
      right = List.drop (index + 1) lst
  in  List.append left (item :: right) 
