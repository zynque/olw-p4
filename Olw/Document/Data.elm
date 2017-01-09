module Olw.Document.Data exposing (
    DocumentData(..),
    NodeReference(..),
    InternalNodeReference(..),
    ExternalNodeReference(..),
    DocumentReference(..)
  )

type DocumentData =
  StringData String |
  IntData Int |
  Reference NodeReference

type NodeReference = Internal InternalNodeReference | External ExternalNodeReference

type InternalNodeReference = InternalNodeReference Int

type ExternalNodeReference = ExternalNodeReference {
  documentReference: DocumentReference,
  nodeId: Int
}

type DocumentReference = DocumentReference {
  url: String,
  version: Int
}
