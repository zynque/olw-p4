import Array
import Dict
import Html exposing (Html, button, div, text, p)
import Html.App as Html
import Html.Events exposing (onClick)
import Olw.Document exposing (..)
import Olw.Detached as Detached exposing (..)
import Olw.DocumentV as DocumentV exposing (..)
import Olw.DocumentVBuilder as DocumentVBuilder exposing (..)
import Olw.DocumentVNav as DocumentVNav exposing (..)

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

detachedDoc = let (n,s,i) = Detached.builderFunctions
        in  n([s("a"),i(3),n([s("b"),i(2)])])

rawDoc = DocumentVBuilder.buildRawDocument detachedDoc
convertedDoc = DocumentVBuilder.buildDocument detachedDoc

updatedDoc = DocumentVNav.updateNodeData 3 (IntData 42) convertedDoc

view : Model -> Html Msg
view model =
  div []
    [ p [] [ text ("detached doc example: " ++ toString detachedDoc) ],
      p [] [ text ("converted to raw: " ++ DocumentVBuilder.showRawDocument rawDoc) ],
      p [] [ text ("converted to docV: " ++ DocumentV.showDocument convertedDoc) ],
      p [] [ text ("updated: " ++ DocumentV.showDocumentResult updatedDoc) ]
    ]
 