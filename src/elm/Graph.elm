module Graph exposing (..)

import AllDict
import Dict as D
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, optional, required)
import Json.Encode as Encode
import Utils exposing ((=>))


type alias Position =
    { x : Int
    , y : Int
    }


type NodeId
    = NodeId Int


ordNodeId : NodeId -> Int
ordNodeId (NodeId n) =
    n


type alias Connection =
    { id : String
    , sourceId : NodeId
    , sourcePort : String
    , targetId : NodeId
    , targetPort : String
    , vertices : List Position
    }


type NodeKind
    = ModelNode String


type alias Node =
    { id : NodeId
    , inPorts : List String
    , outPorts : List String
    , position : Position
    , kind : NodeKind
    }


type alias Graph =
    { nodes : AllDict.AllDict NodeId Node Int
    , connections : D.Dict String Connection
    , idGen : Int
    , uuid : String
    }


nodeIdEncode : NodeId -> Encode.Value
nodeIdEncode (NodeId n) =
    Encode.int n


nodeIdStrEncode : NodeId -> Encode.Value
nodeIdStrEncode (NodeId n) =
    toString n |> Encode.string


encodeConnection : Connection -> Encode.Value
encodeConnection { id, sourceId, sourcePort, targetId, targetPort, vertices } =
    Encode.object
        [ "id" => Encode.string id
        , "sourceId" => nodeIdEncode sourceId
        , "sourcePort" => Encode.string sourcePort
        , "targetId" => nodeIdEncode targetId
        , "targetPort" => Encode.string targetPort
        , "vertices" => Encode.list (List.map encodePosition vertices)
        ]


connectionDecoder : Decode.Decoder Connection
connectionDecoder =
    decode Connection
        |> required "id" Decode.string
        |> required "sourceId" nodeIdDecoder
        |> required "sourcePort" Decode.string
        |> required "targetId" nodeIdDecoder
        |> required "targetPort" Decode.string
        |> optional "vertices" (Decode.list positionDecoder) []


encodePosition : Position -> Encode.Value
encodePosition { x, y } =
    Encode.object
        [ "x" => Encode.int x
        , "y" => Encode.int y
        ]


encodeNodeKind : NodeKind -> Encode.Value
encodeNodeKind (ModelNode id) =
    Encode.object
        [ "kind" => Encode.string "model"
        , "id" => Encode.string id
        ]


encodeNode : Node -> Encode.Value
encodeNode { id, inPorts, outPorts, position, kind } =
    Encode.object
        [ "id" => nodeIdEncode id
        , "inPorts" => (inPorts |> List.map Encode.string |> Encode.list)
        , "outPorts" => (outPorts |> List.map Encode.string |> Encode.list)
        , "position" => encodePosition position
        , "kind" => encodeNodeKind kind
        ]


positionDecoder : Decode.Decoder Position
positionDecoder =
    decode Position |> required "x" Decode.int |> required "y" Decode.int


nodeIdDecoder : Decode.Decoder NodeId
nodeIdDecoder =
    let
        strToInt =
            String.toInt >> (Result.withDefault 0)
    in
        Decode.oneOf [ Decode.int, Decode.string |> Decode.map strToInt ] |> Decode.map NodeId


nodeDecoder : Decode.Decoder Node
nodeDecoder =
    decode Node
        |> required "id" nodeIdDecoder
        |> required "inPorts" (Decode.list Decode.string)
        |> required "outPorts" (Decode.list Decode.string)
        |> required "position" positionDecoder
        |> required "kind" (decode ModelNode |> required "id" Decode.string)


encodeGraph : Graph -> Encode.Value
encodeGraph { nodes, connections, idGen, uuid } =
    Encode.object
        [ "idGen" => Encode.int idGen
        , "uuid" => Encode.string uuid
        , "nodes" => (nodes |> AllDict.values |> List.map encodeNode |> Encode.list)
        , "connections" => (connections |> D.values |> List.map encodeConnection |> Encode.list)
        ]


graphDecoder : Decode.Decoder Graph
graphDecoder =
    let
        graphCtor idGen uuid nodes conns =
            let
                g1 =
                    new uuid

                g2 =
                    { g1 | idGen = idGen }

                g3 =
                    List.foldr (\n g -> addNode n g) g2 nodes

                g4 =
                    List.foldr (\c g -> addConnection c g) g3 conns
            in
                g4
    in
        decode graphCtor
            |> required "idGen" Decode.int
            |> required "uuid" Decode.string
            |> required "nodes" (Decode.list nodeDecoder)
            |> required "connections"
                (Decode.list connectionDecoder)


graphToJson : Graph -> String
graphToJson graph =
    encodeGraph graph |> Encode.encode 0


nodes : Graph -> List Node
nodes graph =
    AllDict.values graph.nodes


connections : Graph -> List Connection
connections graph =
    D.values graph.connections


new : String -> Graph
new uuid =
    { uuid = uuid, nodes = AllDict.empty ordNodeId, connections = D.empty, idGen = 0 }


findNode : Graph -> NodeId -> Maybe Node
findNode { nodes } nodeId =
    case AllDict.get nodeId nodes of
        Nothing ->
            Nothing

        r ->
            r


newNodeId : Graph -> ( NodeId, Graph )
newNodeId graph =
    let
        newId_ =
            graph.idGen + 1

        newId =
            NodeId newId_
    in
        newId => { graph | idGen = newId_ }


newNode : NodeKind -> Graph -> ( Node, Graph )
newNode kind graph =
    let
        ( newId, newGraph ) =
            newNodeId graph

        node =
            { id = newId, kind = kind, inPorts = [], outPorts = [], position = { x = 50, y = 50 } }
    in
        ( node, newGraph )


addNode : Node -> Graph -> Graph
addNode node oldG =
    { oldG | nodes = AllDict.insert node.id node oldG.nodes }


removeNode : NodeId -> Graph -> Graph
removeNode nodeId oldG =
    { oldG
        | nodes = AllDict.remove nodeId oldG.nodes
        , connections = D.filter (\id c -> c.sourceId /= nodeId && c.targetId /= nodeId) oldG.connections
    }


moveNode : NodeId -> Position -> Graph -> Graph
moveNode nodeId pos graph =
    let
        maybeNode =
            findNode graph nodeId |> Maybe.map (\n -> { n | position = pos })
    in
        case maybeNode of
            Nothing ->
                graph

            Just node ->
                addNode node graph


addConnection : Connection -> Graph -> Graph
addConnection conn oldG =
    { oldG | connections = D.insert conn.id conn oldG.connections }


removeConnection : String -> Graph -> Graph
removeConnection connId oldG =
    { oldG | connections = D.remove connId oldG.connections }


connectionsOfNode : NodeId -> Graph -> List Connection
connectionsOfNode nodeId graph =
    let
        conns =
            D.values graph.connections
    in
        List.filter (\conn -> conn.sourceId == nodeId || conn.targetId == nodeId) conns


connectedOutputsOfNode : NodeId -> Graph -> List String
connectedOutputsOfNode nodeId graph =
    let
        conns =
            D.values graph.connections
    in
        List.filter (\conn -> conn.sourceId == nodeId) conns |> List.map .sourcePort


connectedInputsOfNode : NodeId -> Graph -> List String
connectedInputsOfNode nodeId graph =
    let
        conns =
            D.values graph.connections
    in
        List.filter (\conn -> conn.targetId == nodeId) conns |> List.map .targetPort


neighborsOfNode : NodeId -> Graph -> List Node
neighborsOfNode nodeId graph =
    let
        conns =
            connectionsOfNode nodeId graph

        nodes =
            List.filterMap
                (\conn ->
                    if conn.sourceId == nodeId then
                        findNode graph conn.targetId
                    else
                        findNode graph conn.sourceId
                )
                conns
    in
        nodes


{-| Returns the pairs of node ids and model UUIDs corresponding to nodes
 that are actually Models
-}
modelNodes : Graph -> List ( NodeId, String )
modelNodes graph =
    nodes graph
        |> List.map
            (\{ id, kind } ->
                case kind of
                    ModelNode uuid ->
                        id => uuid
            )
