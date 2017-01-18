port module Ports
    exposing
        ( Msg(..)
        , UIGraph
        , showOrHideModal
        , loadHypermodel
        , loadHypermodel2
        , addModelToGraph
        , scaleGraph
        , serializeGraph
        , subscriptions
        )

import State exposing (..)
import Graph exposing (..)
import Json.Encode


type alias UIGraph =
    { canvas : String
    , svg : String
    }


type Msg
    = NewGraph UIGraph
    | NewConnection Connection
    | RemoveConnection String
    | RemoveNode String
    | ShowNode String
    | MoveNode String Position



-- Send to JS:


port modals : { id : String, show : Bool } -> Cmd msg


port loadHypermodel : { readOnly : Bool, jsonStr : String } -> Cmd msg


port loadHypermodel2 : Json.Encode.Value -> Cmd msg


port addNode : { id : String, name : String, ports : { inPorts : List String, outPorts : List String, dynPorts : List String }, position : Graph.Position } -> Cmd msg


port scaleGraph : Float -> Cmd msg


port serializeGraph : () -> Cmd msg



-- Receive from JS:


type alias JointJsConnection =
    { id : String
    , sourceId : String
    , sourcePort : String
    , targetId : String
    , targetPort : String
    , vertices : List Graph.Position
    }


jsConnToConnection : JointJsConnection -> Connection
jsConnToConnection { id, sourceId, sourcePort, targetId, targetPort, vertices } =
    let
        strToNodeId =
            String.toInt >> Result.withDefault 0 >> Graph.NodeId
    in
        { id = id
        , sourceId = strToNodeId sourceId
        , sourcePort = sourcePort
        , targetId = strToNodeId targetId
        , targetPort = targetPort
        , vertices = vertices
        }


port newGraphSignal : (UIGraph -> msg) -> Sub msg


port newConnectionSignal : (JointJsConnection -> msg) -> Sub msg


port moveNodeSignal : ({ node : String, x : Int, y : Int } -> msg) -> Sub msg


port removeConnectionSignal : (String -> msg) -> Sub msg


port removeNodeSignal : (String -> msg) -> Sub msg


port showNodeSignal : (String -> msg) -> Sub msg


subscriptions : State -> Sub Msg
subscriptions state =
    Sub.batch
        [ newGraphSignal NewGraph
        , newConnectionSignal jsConnToConnection |> Sub.map NewConnection
        , moveNodeSignal (\{ node, x, y } -> MoveNode node { x = x, y = y })
        , removeConnectionSignal RemoveConnection
        , removeNodeSignal RemoveNode
        , showNodeSignal ShowNode
        ]


showOrHideModal : Bool -> String -> Cmd msg
showOrHideModal b modalId =
    modals { id = modalId, show = b }


addModelToGraph : Graph.NodeId -> Graph.Position -> State.Model -> Cmd msg
addModelToGraph (Graph.NodeId nodeId) position model =
    let
        inPorts =
            List.map .name model.inPorts

        outPorts =
            List.map .name model.outPorts

        dynPorts =
            model.inPorts ++ model.outPorts |> List.filter .isDynamic |> List.map .name
    in
        addNode
            { id = toString nodeId
            , name = model.title
            , ports = { inPorts = inPorts, outPorts = outPorts, dynPorts = dynPorts }
            , position = position
            }
