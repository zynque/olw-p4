module Tests.Olw.Document.DocumentTests exposing(..)

import Test exposing (..)
import Expect

import Olw.Document.Document as Document exposing (..)
import Olw.Document.Detached as Detached exposing (..)
import Olw.Document.Build as Build exposing (..)
import Array.Hamt as Array


--         tree:           a(5)
--                     /    |     \
--                   b(0)  2(1)   c(4)
--                              /      \
--                            d(2)   3(3)
--
detachedDoc = let (dn, s, i, sdn, idn) = Detached.builderFunctions
              in  dn (s "a", [sdn "b", idn 2, dn (s "c", [sdn "d", idn 3])])


document = Build.buildDocument detachedDoc


documentTest : Test
documentTest =
  describe "Document" [
    test "parents" <|
      \() -> document.parentIds
        |> Expect.equal (Array.fromList [Just 5, Just 5, Just 4, Just 4, Just 5, Nothing]),
    test "get parent" <|
      \() -> Document.parentOf 2 document
        |> Expect.equal (Just 4),
    test "get no parent" <|
      \() -> Document.parentOf 5 document
        |> Expect.equal Nothing,

    test "get children" <|
      \() -> Document.childrenOf 4 document
        |> Expect.equal [2, 3],
    test "get no children" <|
      \() -> Document.childrenOf 3 document
        |> Expect.equal [],

    test "path from root to node" <|
      \() -> Document.pathFromRootTo 2 document
        |> Expect.equal [5, 4, 2],
    test "path from root to root" <|
      \() -> Document.pathFromRootTo 5 document
        |> Expect.equal [5] 
  ]
