module Olw.Document.Detached exposing(..)

import Olw.Document.Data exposing (..)

----------------------------------------------------------------------------
-- Detached nodes
-- "detached" in the sense that these nodes are not yet assigned ids, and do not have history
-- they are also arranged in an actual tree structure, whereas the document
-- is based on an array

type DetachedNode tData = DetachedNode {data: tData, children: List (DetachedNode tData)}

-- compact syntax for creating detached document trees/subtrees
-- for example, here's how to bring the builder syntax into a local scope:
--
-- detachedDoc = let (dn, s, i, sdn, idn) = Detached.builderFunctions
--               in  dn (s "a", [sdn "b", idn 2, dn (s "c", [sdn "d", idn 3])])
--
-- generates the tree:      a
--                        / |  \
--                       b  2  c
--                            / \
--                           d   3

builderFunctions =
  let dn (val, children) = DetachedNode {data = val, children = children}
      s val = StringData val
      i val = IntData val
      sdn val = dn (s val, [])
      idn val = dn (i val, [])
  in  (dn, s, i, sdn, idn)
