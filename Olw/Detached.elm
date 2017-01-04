module Olw.Detached exposing(..)

import Olw.Document as Document exposing (..)

----------------------------------------------------------------------------
-- Detached nodes
-- "detached" in the sense that these nodes are not yet assigned ids, and do not have history

type DetachedNode tData = DetachedDataNode tData | DetachedInternalNode (List (DetachedNode tData))

-- compact syntax for creating detached document trees/subtrees
-- for example, here's how to bring the builder syntax into a local scope:
--
-- myDoc = let (n,s,i) = builderFunctions
--         in  n([s("a"),i(3),n([s("b"),i(2)])])
--
-- generates the tree:      *
--                        / | \
--                       a  3  *
--                            / \
--                           b   2

builderFunctions =
  let n children = DetachedInternalNode children
      s val = DetachedDataNode (StringNode val)
      i val = DetachedDataNode (IntNode val) 
  in  (n, s, i)
