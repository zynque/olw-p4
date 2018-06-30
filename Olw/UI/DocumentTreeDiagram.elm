module Olw.UI.DocumentTreeDiagram exposing (..)

import Olw.Document.Data exposing (..)
import Olw.Document.Document exposing (..)
import Olw.Document.Show exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


-- Resource: https://llimllib.github.io/pymag-trees/


type alias Point =
    { x : Int
    , y : Int
    }


type alias LabeledCircle =
    { point : Point
    , insidePrimaryLabel : String
    , insideSecondaryLabel : String
    , outsideLabel : String
    }


labeledCircle coords insidePrimaryLabel insideSecondaryLabel outsideLabel =
    g []
        [ circle
            [ cx (toString coords.x)
            , cy (toString coords.y)
            , r "30"
            , fill "white"
            , stroke "black"
            , strokeWidth "3"
            ]
            []
        , text_
            [ x (toString coords.x)
            , y (toString coords.y)
            , textAnchor "middle"
            , fontFamily "sans-serif"
            , fontSize "1em"
            , fill "black"
            ]
            [ text insidePrimaryLabel ]
        , text_
            [ x (toString coords.x)
            , y (toString (coords.y + 10))
            , textAnchor "middle"
            , fontFamily "sans-serif"
            , fontSize ".6em"
            , fill "black"
            ]
            [ text insideSecondaryLabel ]
        , text_
            [ x (toString coords.x)
            , y (toString (coords.y + 45))
            , textAnchor "middle"
            , fontFamily "sans-serif"
            , fontSize ".7em"
            , fill "red"
            ]
            [ text outsideLabel ]
        ]


svgExample doc =
    svg [ width "600", height "400", viewBox "0 0 600 400" ]
        [ rect [ x "10", y "10", width "100", height "100", rx "15", ry "15" ] []
        , line [ x1 "350", y1 "30", x2 "500", y2 "60", stroke "black", strokeWidth "3" ] []
        , labeledCircle { x = 260, y = 60 } "123" "note" "caption"
        , showDocAsSvgTree doc
        ]



--showVersionedNodeData : Node NodeData -> String
--showVersionedNodeData node = showNodeData node.data
--nodeAsSvg : VersionedNode NodeData -> Svg msg
--nodeAsSvg versionedNode xPos yPos =
--  let label = showNodeData versionedNode
--  in  circle [] []


showDocAsSvgTree : Document NodeData -> Svg msg
showDocAsSvgTree doc =
    let
        children =
            childrenOf doc.rootId doc

        n =
            labeledCircle { x = 260, y = 150 } (toString doc.rootId) (toString children) ""
    in
    n



-- doc tree drawing (start simple!)
-- recursively, for each node, starting at root:
--  * for each child associate an index from 0 to n, where n is the number of children
--  * for each child associate a width, which is the sum of widths of its children,
--    width of 1 for a leaf node
--  * for this node set its position to half its width
--  * for each child, set its position to half its width plus all siblings/cousins widths on left
-- offset = padding on left to accomodate siblings of this node (sum of node widths on left)


widthAndPositionOfNode : Int -> Int -> Document NodeData -> ( Int, Int )
widthAndPositionOfNode offset nodeId doc =
    let
        children =
            childrenOf nodeId doc

        childrenWidthsAndPositions =
            List.scanl

        width =
            0

        position =
            0
    in
    ( width, position )
