module Olw.Document.WorkingDocument exposing(
    WorkingDocument(..),
    parentOf, childrenOf, pathFromRootTo
  )

import Array exposing (Array)
import String exposing (join)
import Olw.Document.Document as Document exposing (..)

-- additional document information, such as parent id of each node, allows for efficient navigation
-- and updates to a document

type WorkingDocument tData = WorkingDocument {
  document: Document tData,

  -- indexed by node id, gives that node's parent id
  parentIds: Array (Maybe Int)
  
  -- indexed by node id, gives the 0-based index of that node within its parent's list of children
  -- positions: Array (Maybe Int)
}

getVersionedNode : Int -> WorkingDocument tData -> Maybe (VersionedNode tData)
getVersionedNode nodeId wdoc =
  let (WorkingDocument {document}) = wdoc
      (Document {rootId, versionedNodes}) = document
  in  Array.get nodeId versionedNodes

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
