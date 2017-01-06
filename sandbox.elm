import Array
import Dict
import Html exposing (Html, button, div, text, p)
import Html.Events exposing (onClick)
import Olw.Document exposing (..)
import Olw.NavDoc as NavDoc exposing (..)


main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model = Int


model : Model
model =
  0



-- UPDATE


type Msg
  = Increment
  | Decrement
  | AddNode


update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

    AddNode ->
      model


-- VIEW

n0 = DataNode "a"
n1 = DataNode "b"
n2 = InternalNode { childIndices = [0, 1] }
n3 = DataNode "c"
n4 = InternalNode { childIndices = [2, 3] }
n5 = DataNode "d"
n6 = InternalNode { childIndices = [4, 5] }
ns = Array.fromList [n0, n1, n2, n3, n4, n5, n6]
d = Document { rootId = 6, nodes = ns }
nd = NavDoc.fromDocument d

newNode = DataNode "new"

updatedExample =
  let result = NavDoc.update 0 newNode nd
  in case result of
    Ok (NavDoc {doc, parents, relativeIndices}, oldPath, newPath) ->
     showDocument doc ++ "  ~~  " ++ toString oldPath ++ "  ~~  " ++ toString newPath
    Err e -> e

updatedExampleNavDoc = NavDoc.update 0 newNode nd

view : Model -> Html Msg
view model =
  div []
    [ p [] [ button [ onClick Decrement ] [ text "-" ] ]
    , div [] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    , p [] [ text (showDocument d) ]
    , p [] [ text (toString (nd)) ]
    , p [] [ text ("parent of 2: " ++ toString (NavDoc.parentOf 2 nd)) ]
    , p [] [ text ("children of 2: " ++ toString (NavDoc.childrenOf 2 nd)) ]
    , p [] [ text ("pathToRoot 0: " ++ toString (NavDoc.pathToRoot 0 nd)) ]
    , p [] [ text ("updated: " ++ updatedExample) ]
    , p [] [ text ("updatedNavDoc: " ++ toString updatedExampleNavDoc) ]
    ]
 