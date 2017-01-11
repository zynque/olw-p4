import Array
import Dict
import Html exposing (Html, button, div, text, p)
import Html.Events exposing (onClick)
import Olw.Document.Data exposing (..)
import Olw.Document.Detached as Detached exposing (..)
import Olw.Document.Build as Build exposing (..)
import Olw.Document.Show as Show exposing (..)
import Olw.Document.Edit as Edit exposing (..)

-- sandbox.elm & sandbox2.elm to be deprecated and replaced by this one
-- big re-organization and clean-up in progress

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
update msg model = model


-- VIEW

showLines lines =
  let pars = List.map (\l -> p [] [ text l ]) lines
  in  div [] pars

showDoc d = showLines (Show.showDocument d)
showWDoc d = showLines (Show.showWorkingDocument d)
showDocR d = showLines (Show.showResult Show.showDocument d)
showWDocR d = showLines (Show.showResult Show.showWorkingDocument d)


-- samples

detachedDoc = let (n,s,i) = Detached.builderFunctions
              in  n[s "a", i 3, n[s "b", i 2]]

attachment = let (n,s,i) = Detached.builderFunctions
             in  n[i 8, i 9]

doc = Build.buildDocument detachedDoc
workingDoc = Build.buildWorkingDocument doc

doc2 = Edit.updateNodeData 3 (IntData 42) workingDoc

wd2 = Build.buildWorkingDocument (Build.buildDocument detachedDoc)
wd2u = wd2
         |> Edit.insertNode attachment 4 1
         |> Result.andThen (Edit.deleteNode 1)

-- view

view : Model -> Html Msg
view model =
  div []
    [ showDoc doc,
      p [] [ text "." ],
      showWDoc workingDoc,
      p [] [ text "." ],
      showWDocR doc2,
      p [] [ text "." ],
      showWDocR wd2u
    ]
 