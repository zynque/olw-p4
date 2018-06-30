module Olw.Document.Detached exposing (..)

import Olw.Document.Data exposing (..)


----------------------------------------------------------------------------
-- Detached data trees
-- "detached" in the sense that these nodes are not yet assigned ids, and do not have history
-- they are also arranged in an actual tree structure, whereas the document
-- is based on an array


type alias DetachedNode tData =
    { data : tData, detachedChildren : DetachedChildren tData }


type DetachedChildren tData
    = DetachedChildren (List (DetachedNode tData))



-- compact syntax for creating detached document trees/subtrees for testing
-- for example, here's how to bring the builder syntax into a local scope:
--
-- abbreviations:
--              n  : detached node
--              s  : string data
--              i  : int data
--              sl : string leaf node
--              il : int leaf node
--
-- detachedDoc = let (n, s, i, sl, il) = Detached.builderFunctions
--               in  n (s "a", [sl "b", il 2, n (s "c", [sl "d", il 3])])
--
-- generates the tree:      a
--                        / |  \
--                       b  2  c
--                            / \
--                           d   3


builderFunctions =
    let
        n ( val, children ) =
            { data = val, detachedChildren = DetachedChildren children }

        s val =
            StringData val

        i val =
            IntData val

        sl val =
            n ( s val, [] )

        il val =
            n ( i val, [] )
    in
    ( n, s, i, sl, il )
