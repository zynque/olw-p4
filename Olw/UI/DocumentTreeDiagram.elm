module Olw.UI.DocumentTreeDiagram exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


-- Resource: https://llimllib.github.io/pymag-trees/

svgExample =
      svg [ width "600", height "200", viewBox "0 0 600 200"] [
        rect [ x "10", y "10", width "100", height "100", rx "15", ry "15" ] [],
        circle [ cx "260", cy "60", r "30", fill "white", stroke "black", strokeWidth "5" ] [],
        line [ x1 "350", y1 "30", x2 "500", y2 "60", stroke "black", strokeWidth "3" ] []
      ]
