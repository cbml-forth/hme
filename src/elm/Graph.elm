module Graph exposing (..)

import Dict as D
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required, decode, optional)


type alias Position =
    { x : Int
    , y : Int
    }


type alias Connection =
    { id : String
    , sourceId : String
    , sourcePort : String
    , targetId : String
    , targetPort : String
    , vertices : List Position
    }


encodeConnection : Connection -> Encode.Value
encodeConnection { id, sourceId, sourcePort, targetId, targetPort, vertices } =
    Encode.object
        [ ( "id", Encode.string id )
        , ( "sourceId", Encode.string sourceId )
        , ( "sourcePort", Encode.string sourcePort )
        , ( "targetId", Encode.string targetId )
        , ( "targetPort", Encode.string targetPort )
        , ( "vertices", Encode.list (List.map encodePosition vertices) )
        ]


connectionDecoder : Decode.Decoder Connection
connectionDecoder =
    decode Connection
        |> required "id" Decode.string
        |> required "sourceId" Decode.string
        |> required "sourcePort" Decode.string
        |> required "targetId" Decode.string
        |> required "targetPort" Decode.string
        |> optional "vertices" (Decode.list positionDecoder) []


type NodeKind
    = ModelNode String


encodePosition : Position -> Encode.Value
encodePosition { x, y } =
    Encode.object
        [ ( "x", Encode.int x )
        , ( "y", Encode.int y )
        ]


encodeNodeKind : NodeKind -> Encode.Value
encodeNodeKind (ModelNode id) =
    Encode.object
        [ ( "kind", Encode.string "model" )
        , ( "id", Encode.string id )
        ]


type alias Node =
    { id : String
    , inPorts : List String
    , outPorts : List String
    , position : Position
    , kind : NodeKind
    }


encodeNode : Node -> Encode.Value
encodeNode { id, inPorts, outPorts, position, kind } =
    Encode.object
        [ ( "id", Encode.string id )
        , ( "inPorts", inPorts |> List.map Encode.string |> Encode.list )
        , ( "outPorts", outPorts |> List.map Encode.string |> Encode.list )
        , ( "position", encodePosition position )
        , ( "kind", encodeNodeKind kind )
        ]


positionDecoder : Decode.Decoder Position
positionDecoder =
    decode Position |> required "x" Decode.int |> required "y" Decode.int


nodeDecoder : Decode.Decoder Node
nodeDecoder =
    decode Node
        |> required "id" Decode.string
        |> required "inPorts" (Decode.list Decode.string)
        |> required "outPorts" (Decode.list Decode.string)
        |> required "position" positionDecoder
        |> required "kind" (decode ModelNode |> required "id" Decode.string)


type alias Graph =
    { nodes : D.Dict String Node
    , connections : D.Dict String Connection
    , idGen : Int
    , uuid : String
    }


(:::) : a -> b -> ( a, b )
(:::) =
    (,)


encodeGraph : Graph -> Encode.Value
encodeGraph { nodes, connections, idGen, uuid } =
    Encode.object
        [ "idGen" ::: Encode.int idGen
        , "uuid" ::: Encode.string uuid
        , ( "nodes", nodes |> D.values |> List.map encodeNode |> Encode.list )
        , ( "connections", connections |> D.values |> List.map encodeConnection |> Encode.list )
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
    D.values graph.nodes


connections : Graph -> List Connection
connections graph =
    D.values graph.connections


new : String -> Graph
new uuid =
    { uuid = uuid, nodes = D.empty, connections = D.empty, idGen = 0 }


findNode : Graph -> String -> Maybe Node
findNode { nodes } nodeId =
    case D.get nodeId nodes of
        Nothing ->
            Nothing

        r ->
            r


newNodeId : Graph -> ( String, Graph )
newNodeId graph =
    let
        newId =
            graph.idGen + 1
    in
        ( toString newId, { graph | idGen = newId } )


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
    { oldG | nodes = D.insert node.id node oldG.nodes }


removeNode : String -> Graph -> Graph
removeNode nodeId oldG =
    { oldG
        | nodes = D.remove nodeId oldG.nodes
        , connections = D.filter (\id c -> c.sourceId /= nodeId && c.targetId /= nodeId) oldG.connections
    }


moveNode : String -> Position -> Graph -> Graph
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


connectionsOfNode : String -> Graph -> List Connection
connectionsOfNode nodeId graph =
    let
        conns =
            D.values graph.connections
    in
        List.filter (\conn -> conn.sourceId == nodeId || conn.targetId == nodeId) conns


neighborsOfNode : String -> Graph -> List Node
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
