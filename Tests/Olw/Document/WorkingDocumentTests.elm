module Tests.Olw.Document.WorkingDocumentTests exposing(..)

import Test exposing (..)
import Expect

import Olw.Document.WorkingDocument as WorkingDocument exposing (..)
import Olw.Document.Detached as Detached exposing (..)
import Olw.Document.Build as Build exposing (..)

--         tree:           a(5)
--                     /    |     \
--                   b(0)  2(1)   c(4)
--                              /      \
--                            d(2)   3(3)

detachedDoc = let (dn, s, i, sdn, idn) = Detached.builderFunctions
              in  dn (s "a", [sdn "b", idn 2, dn (s "c", [sdn "d", idn 3])])
doc = Build.buildDocument detachedDoc
workingDoc = Build.buildWorkingDocument doc

workingDocumentTest : Test
workingDocumentTest =
  describe "WorkingDocument" [
    test "get parent" <|
      \() -> WorkingDocument.parentOf 2 workingDoc
        |> Expect.equal (Just 4),
    test "get no parent" <|
      \() -> WorkingDocument.parentOf 5 workingDoc
        |> Expect.equal Nothing,

    test "get children" <|
      \() -> WorkingDocument.childrenOf 4 workingDoc
        |> Expect.equal [2, 3],
    test "get no children" <|
      \() -> WorkingDocument.childrenOf 3 workingDoc
        |> Expect.equal [],

    test "path from root to node" <|
      \() -> WorkingDocument.pathFromRootTo 2 workingDoc
        |> Expect.equal [5, 4, 2],
    test "path from root to root" <|
      \() -> WorkingDocument.pathFromRootTo 5 workingDoc
        |> Expect.equal [5] 
  ]
