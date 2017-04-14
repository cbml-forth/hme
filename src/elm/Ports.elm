port module Ports
    exposing
        ( Msg(..)
        , UIGraph
        , showOrHideModal
        , loadHypermodel
        , addModelToGraph
        , scaleGraph
        , serializeGraph
        , subscriptions
        )

import State exposing (..)
import Graph exposing (..)
import Json.Encode
import Json.Decode


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
    | Notification Json.Decode.Value



-- Send to JS:


port modals : { id : String, show : Bool } -> Cmd msg


port loadHypermodel : Json.Encode.Value -> Cmd msg


type alias Param =
    { name : String
    , dataType : String
    , isDynamic : Bool
    }


port addNode : { id : String, name : String, ports : { inPorts : List Param, outPorts : List Param }, position : Graph.Position } -> Cmd msg


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


port notificationSignal : (Json.Decode.Value -> msg) -> Sub msg


subscriptions : State -> Sub Msg
subscriptions state =
    Sub.batch
        [ newGraphSignal NewGraph
        , newConnectionSignal jsConnToConnection |> Sub.map NewConnection
        , moveNodeSignal (\{ node, x, y } -> MoveNode node { x = x, y = y })
        , removeConnectionSignal RemoveConnection
        , removeNodeSignal RemoveNode
        , showNodeSignal ShowNode
        , notificationSignal Notification
        ]


showOrHideModal : Bool -> String -> Cmd msg
showOrHideModal b modalId =
    modals { id = modalId, show = b }


addModelToGraph : Graph.NodeId -> Graph.Position -> State.Model -> Cmd msg
addModelToGraph (Graph.NodeId nodeId) position model =
    let
        mkParam : State.ModelInOutput -> Param
        mkParam { name, dataType, isDynamic } =
            { name = name, dataType = dataType, isDynamic = isDynamic }

        inPorts =
            List.map mkParam model.inPorts

        outPorts =
            List.map mkParam model.outPorts
    in
        addNode
            { id = toString nodeId
            , name = model.title
            , ports = { inPorts = inPorts, outPorts = outPorts }
            , position = position
            }
