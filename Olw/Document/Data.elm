-- The data contained in an OLW node:
--   a primitive type such as string or int,
--   or a reference to another node in this document
--   or a reference to a node in another document


module Olw.Document.Data
    exposing
        ( ExternalNodeReference
        , NodeData(..)
        )


type NodeData
    = StringData String
    | IntData Int
    | FloatData Float
    | InternalNodeRef Int
    | ExternalNodeRef ExternalNodeReference


type alias ExternalNodeReference =
    { documentUrl : String
    , documentVersionId : Int
    , nodeId : Int
    }
