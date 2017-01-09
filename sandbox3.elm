import Array
import Dict
import Html exposing (Html, button, div, text, p)
import Html.Events exposing (onClick)
import Olw.Document.Data exposing (..)
import Olw.Document.Detached as Detached exposing (..)
import Olw.Document.Build as Build exposing (..)
import Olw.Document.Show as Show exposing (..)

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

detachedDoc = let (n,s,i) = Detached.builderFunctions
              in  n[s "a", i 3, n[s "b", i 2]]

doc = Build.buildDocument detachedDoc
workingDoc = Build.buildWorkingDocument doc

showLines lines =
  let pars = List.map (\l -> p [] [ text l ]) lines
  in  div [] pars

showDoc d = showLines (Show.showDocument d)
showDocR d = showLines (Show.showDocumentResult d)


view : Model -> Html Msg
view model =
  div []
    [ p [] [ text ("xyz: " ++ "123") ],
      p [] [ text (toString (Reference (Internal (InternalNodeReference 3)))) ],
      p [] [ text (toString detachedDoc) ],
      p [] [ text (toString doc) ],
      p [] [ text (toString workingDoc) ],
      showDoc doc
    ]
 