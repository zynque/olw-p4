module Main exposing (..)

import Array
import Dict
import Html exposing (Html, button, div, p, text)
import Html.Events exposing (onClick)
import Olw.Document.Build as Build exposing (..)
import Olw.Document.Data exposing (..)
import Olw.Document.Detached as Detached exposing (..)
import Olw.Document.Edit as Edit exposing (..)
import Olw.Document.Show as Show exposing (..)
import Olw.Manifesto.Manifesto exposing (manifesto)


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    Int


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
    model



-- VIEW


showLines lines =
    let
        pars =
            List.map (\l -> p [] [ text l ]) lines
    in
    div [] pars


showWDoc d =
    showLines ("Document" :: Show.showDocument d)


showWDocR d =
    showLines ("Document" :: Show.showResult Show.showDocument d)



-- samples


detachedDoc =
    let
        ( dn, s, i, sdn, idn ) =
            Detached.builderFunctions
    in
    dn ( s "a", [ sdn "b", idn 42, dn ( s "c", [ sdn "d", idn 43 ] ) ] )


attachment =
    let
        ( dn, s, i, sdn, idn ) =
            Detached.builderFunctions
    in
    dn ( i 2, [ idn 8, idn 9 ] )


workingDoc =
    Build.buildDocument detachedDoc


doc2 =
    Edit.updateNodeData 3 (IntData 42) workingDoc


wd2 =
    Build.buildDocument detachedDoc


wd2u =
    wd2
        |> Edit.insertNode attachment 4 1
        |> Result.andThen (Edit.cutNode 1)


wd3 =
    Build.buildDocument detachedDoc


wd3u =
    wd3
        |> Edit.cutNode 0
        |> Result.andThen (Edit.pasteNode 0 4 1)


wd3failedUpdate =
    wd3
        |> Edit.pasteNode 0 4 1


view : Model -> Html Msg
view model =
    div []
        [ svgExample workingDoc
        , p [] [ text "." ]
        , p [] [ text (toString detachedDoc) ]
        , p [] [ text "." ]
        , showWDoc workingDoc
        , p [] [ text "." ]
        , showWDocR doc2
        , p [] [ text "." ]
        , showWDocR wd2u
        , p [] [ text "." ]
        , showWDocR wd3u
        , p [] [ text "." ]
        , showWDocR wd3failedUpdate
        , p [] [ text "." ]
        ]
