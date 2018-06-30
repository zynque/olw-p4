module Olw.Document.ElmCoreExt
    exposing
        ( arrayInsertBefore
        , listInsertBefore
        , listRemoveAt
        , listSetAt
        , maybeFlatten
        )

import Array.Hamt as Array exposing (Array)


arrayInsertBefore : Int -> t -> Array t -> Array t
arrayInsertBefore index item arr =
    let
        left =
            Array.slice 0 index arr

        right =
            Array.slice index (Array.length arr) arr
    in
    Array.append (Array.push item left) right


listInsertBefore : Int -> t -> List t -> List t
listInsertBefore index item lst =
    let
        left =
            List.take index lst

        right =
            List.drop index lst
    in
    List.append left (item :: right)


listRemoveAt : Int -> List t -> List t
listRemoveAt index lst =
    let
        left =
            List.take index lst

        right =
            List.drop (index + 1) lst
    in
    List.append left right


listSetAt : Int -> t -> List t -> List t
listSetAt index item lst =
    let
        left =
            List.take index lst

        right =
            List.drop (index + 1) lst
    in
    List.append left (item :: right)


maybeFlatten : Maybe (Maybe t) -> Maybe t
maybeFlatten m =
    Maybe.withDefault Nothing m
