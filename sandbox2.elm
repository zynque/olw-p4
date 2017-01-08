import Array
import Dict
import Html exposing (Html, button, div, text, p)
--import Html.App as Html
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

tail = let (n,s,i) = Detached.builderFunctions
       in  n(List.repeat 32 (i 0))

detachedDoc = let (n,s,i) = Detached.builderFunctions
              in  n[s "a", i 3, n[s "b", i 2]]

rawDoc = DocumentVBuilder.buildRawDocument detachedDoc
convertedDoc = DocumentVBuilder.buildDocument detachedDoc

updatedDoc = DocumentVNav.updateNodeData 3 (IntData 42) convertedDoc

doc2 =
  let (n,s,i) = Detached.builderFunctions
      d = n[i 1, n[i 3, i 4],i 2]
  in DocumentVBuilder.buildDocument d

addedSubtree =
  let (n,s,i) = Detached.builderFunctions
      detached = n[i 5,i 6]
      doc3 = DocumentVNav.insertNode detached 3 1 doc2
  in  case doc3 of
        Ok d -> DocumentVNav.deleteNode 2 d
        e -> e

showLines lines =
  let pars = List.map (\l -> p [] [ text l ]) lines
  in  div [] pars

showDoc d = showLines (DocumentV.showDocument d)
showDocR d = showLines (DocumentV.showDocumentResult d)

view : Model -> Html Msg
view model =
  div []
    [ p [] [ text ("detached doc example: " ++ toString detachedDoc) ],
      p [] [ text "." ],
      p [] [ text ("converted to raw: " ++ DocumentVBuilder.showRawDocument rawDoc) ],
      p [] [ text "." ],
      p [] [ text ("converted to docV: ") ],
      showDoc convertedDoc,
      p [] [ text "." ],
      p [] [ text ("data updated at node 3: ") ],
      showDocR updatedDoc,
      p [] [ text "." ],
      p [] [ text ("d2: ") ],
      showDoc doc2,
      p [] [ text "." ],
      p [] [ text ("addedSubtree: ") ],
      showDocR addedSubtree,
      p [] [ text "." ],
      p [] [ text (toString (DocumentVNav.listRemoveAt 1 [1,2,3])) ],

      p [] []
    ]
 