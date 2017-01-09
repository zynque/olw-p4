module Olw.Document.WorkingDocument exposing(
    WorkingDocument(..)
  )

import Array exposing (Array)
import String exposing (join)
import Olw.Document.Document exposing (..)

-- additional document information, such as parent id of each node, allows for efficient navigation
-- and updates to a document

type WorkingDocument tData = WorkingDocument {
  document: Document tData,

  -- indexed by node id, gives that node's parent id
  parentIds: Array (Maybe Int)
  
  -- indexed by node id, gives the 0-based index of that node within its parent's list of children
  -- positions: Array (Maybe Int)
}
