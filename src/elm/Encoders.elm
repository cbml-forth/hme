module Encoders exposing (encodeHypermodel, encodeModelParameter, encodeModelInputWithValue)

import Graph
import Json.Encode as Encode
import Number.Expanded exposing (Expanded)
import State
import Number.Expanded exposing (..)
import List
import Utils exposing ((=>))


encodeHypermodel : Bool -> State.Hypermodel -> Encode.Value
encodeHypermodel isStronglyCoupled { title, id, description, canvas, graph, svgContent } =
    Encode.object
        [ ( "title", Encode.string title )
        , ( "uuid", Encode.string id )
        , ( "description", Encode.string description )
        , ( "canvas", Encode.string canvas )
        , ( "graph", Graph.encodeGraph graph )
        , ( "svg_content", Encode.string svgContent )
        , ( "isStronglyCoupled", Encode.bool isStronglyCoupled )
        ]


encodeExpanded : Expanded number -> String
encodeExpanded e =
    case e of
        Finite a ->
            toString a

        _ ->
            ""


encodeMaybeOrNull : (a -> Encode.Value) -> Maybe a -> Encode.Value
encodeMaybeOrNull encoder m =
    Maybe.map encoder m |> Maybe.withDefault Encode.null


encodeValueRange : State.ValueRange -> Encode.Value
encodeValueRange ( low, high ) =
    let
        str =
            encodeExpanded low ++ "_" ++ encodeExpanded high
    in
        Encode.string str


encodeModelParameter : Bool -> State.ModelInOutput -> Encode.Value
encodeModelParameter isOutput { repoId, uuid, name, isDynamic, isMandatory, dataType, units, description, range, defaultValue, semtype } =
    let
        boolToInt b =
            if b then
                1
            else
                0
    in
        Encode.object
            [ ( "id", Encode.int repoId )
            , ( "uuid", Encode.string uuid )
            , ( "name", Encode.string name )
            , ( "is_output", Encode.int (boolToInt isOutput) )
            , ( "is_mandatory", Encode.int (boolToInt isMandatory) )
            , ( "data_type", Encode.string dataType )
            , ( "semtype", Encode.list (List.map Encode.string semtype) )
            , ( "description", Encode.string description )
            , ( "default_value", encodeMaybeOrNull Encode.string defaultValue )
            , ( "data_range", encodeMaybeOrNull encodeValueRange range )
            , ( "unit", Encode.string units )
            ]


encodeModelInputWithValue : State.ModelInputWithValue -> Encode.Value
encodeModelInputWithValue (State.ModelInputWithValue input value) =
    Encode.object
        [ "param" => encodeModelParameter False input
        , "value" => Encode.string value
        ]
