module Olw.Document.WorkingDocument exposing(
    WorkingDocument(..),
    getVersionedNode, parentOf, childrenOf, pathFromRootTo
  )

import Array.Hamt as Array exposing (Array)
import String exposing (join)
import Olw.Document.Document as Document exposing (..)

-- a WorkingDocument is a Document augmented with additional information,
-- such as parent id of each node, allowing for efficient navigation
-- and updates to a document

type WorkingDocument tData = WorkingDocument {
  document: Document tData,

  -- indexed by node id, gives that node's parent id
  parentIds: Array (Maybe Int)
  
  -- indexed by node id, gives the 0-based index of that node within its parent's list of children
  -- positions: Array (Maybe Int)
}

getVersionedNode : Int -> WorkingDocument tData -> Maybe (Node tData)
getVersionedNode nodeId wdoc =
  let (WorkingDocument {document}) = wdoc
  in  Array.get nodeId document.nodes

parentOf : Int -> WorkingDocument tData -> Maybe (Int)
parentOf nodeId wdoc =
  let (WorkingDocument {parentIds}) = wdoc
  in  case Array.get nodeId parentIds of
      Just maybeId -> maybeId
      Nothing -> Nothing

childrenOf : Int -> WorkingDocument tData -> List Int
childrenOf nodeId wdoc =
  let (WorkingDocument {document, parentIds}) = wdoc
  in  Document.childrenOf nodeId document

pathFromRootTo : Int -> WorkingDocument tData -> List Int
pathFromRootTo nodeId wdoc = pathToRootFrom nodeId wdoc |> List.reverse

pathToRootFrom : Int -> WorkingDocument tData -> List Int
pathToRootFrom nodeId wdoc =
  case (parentOf nodeId wdoc) of
    Nothing -> [nodeId]
    Just parentId -> nodeId :: (pathToRootFrom parentId wdoc)
