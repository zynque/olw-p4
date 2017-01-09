module Olw.Document.Detached exposing(..)

import Olw.Document.Data exposing (..)

----------------------------------------------------------------------------
-- Detached nodes
-- "detached" in the sense that these nodes are not yet assigned ids, and do not have history
-- they are also arranged in an actual tree structure, whereas the document
-- is based on an array

type DetachedNode tData = DetachedLeaf tData | DetachedNode (List (DetachedNode tData))

-- compact syntax for creating detached document trees/subtrees
-- for example, here's how to bring the builder syntax into a local scope:
--
-- myDoc = let (n,s,i) = builderFunctions
--         in  n [s "a",i 3,n [s "b", i 2]]
--
-- generates the tree:      *
--                        / | \
--                       a  3  *
--                            / \
--                           b   2

builderFunctions =
  let n children = DetachedNode children
      s val = DetachedLeaf (StringData val)
      i val = DetachedLeaf (IntData val) 
  in  (n, s, i)
