module Olw.UI.DocumentTreeDiagram exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Olw.Document.Show exposing (..)
import Olw.Document.Data exposing (..)
import Olw.Document.Document exposing (..)
import Olw.Document.WorkingDocument exposing (..)


-- Resource: https://llimllib.github.io/pymag-trees/


labeledCircle coords insidePrimaryLabel insideSecondaryLabel outsideLabel =
  g [] [
    circle [ cx (toString coords.x),
             cy (toString coords.y),
             r "30",
             fill "white",
             stroke "black",
             strokeWidth "3" ] [],
    text_ [ x (toString coords.x),
            y (toString coords.y),
            textAnchor "middle",
            fontFamily "sans-serif",
            fontSize "1em",
            fill "black" ] [text insidePrimaryLabel],
    text_ [ x (toString coords.x),
            y (toString (coords.y + 10)),
            textAnchor "middle",
            fontFamily "sans-serif",
            fontSize ".6em",
            fill "black" ] [text "comment"],
    text_ [ x (toString coords.x),
            y (toString (coords.y + 45)),
            textAnchor "middle",
            fontFamily "sans-serif",
            fontSize ".7em",
            fill "red" ] [text "caption"]
  ]

svgExample =
      svg [ width "600", height "400", viewBox "0 0 600 400"] 
      [
        rect [ x "10", y "10", width "100", height "100", rx "15", ry "15" ] [],
        line [ x1 "350", y1 "30", x2 "500", y2 "60", stroke "black", strokeWidth "3" ] [],
        (labeledCircle { x = 260, y = 60 } "123" "note" "caption"),
        (labeledCircle { x = 260, y = 150 } "30" "acircle" "anote")
      ] 


showVersionedNodeData : VersionedNode NodeData -> String
showVersionedNodeData versionedNode =
  let (VersionedNode {version, node}) = versionedNode
      (Node {data, childIds}) = node
  in  showNodeData data

--nodeAsSvg : VersionedNode NodeData -> Svg msg
--nodeAsSvg versionedNode xPos yPos =
--  let label = showNodeData versionedNode
--  in  circle [] [] 
