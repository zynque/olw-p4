module Tests.Olw.Document.DocumentTests exposing (..)

import Array.Hamt as Array
import Expect
import Olw.Document.Build as Build exposing (..)
import Olw.Document.Detached as Detached exposing (..)
import Olw.Document.Document as Document exposing (..)
import Test exposing (..)


--         tree:           a(5)
--                     /    |     \
--                   b(0)  2(1)   c(4)
--                              /      \
--                            d(2)   3(3)


detachedDoc =
    let
        ( n, s, i, sl, il ) =
            Detached.builderFunctions
    in
    n ( s "a", [ sl "b", il 2, n ( s "c", [ sl "d", il 3 ] ) ] )


document =
    Build.buildDocument detachedDoc


documentTest : Test
documentTest =
    describe "Document"
        [ test "get parent" <|
            \() ->
                Document.parentOf 2 document
                    |> Expect.equal (Just 4)
        , test "get no parent" <|
            \() ->
                Document.parentOf 5 document
                    |> Expect.equal Nothing
        , test "get children" <|
            \() ->
                Document.childrenOf 4 document
                    |> Expect.equal [ 2, 3 ]
        , test "get no children" <|
            \() ->
                Document.childrenOf 3 document
                    |> Expect.equal []
        , test "path from root to node" <|
            \() ->
                Document.pathFromRootTo 2 document
                    |> Expect.equal [ 5, 4, 2 ]
        , test "path from root to root" <|
            \() ->
                Document.pathFromRootTo 5 document
                    |> Expect.equal [ 5 ]
        ]
