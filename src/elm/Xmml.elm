module Xmml exposing (toXmml, toXmmlString)

import String
import List
import Graph
import State exposing (Model, ModelInOutput, findΜodelByUUID)
import String.Extra
import Utils exposing ((=>), list)


type alias XMLAttr =
    ( String, String )


type XMLNode
    = XMLNode
        { tag : String
        , attributes : List XMLAttr
        , children : List XMLNode
        }
    | Text String


emptyNode : String -> XMLNode
emptyNode tag =
    XMLNode { tag = tag, attributes = [], children = [] }


nodeChildren : String -> List XMLNode -> XMLNode
nodeChildren tag children =
    XMLNode { tag = tag, attributes = [], children = children }


nodeAttrs : String -> List XMLAttr -> XMLNode
nodeAttrs tag attrs =
    XMLNode { tag = tag, attributes = attrs, children = [] }


createNode : String -> List XMLAttr -> List XMLNode -> XMLNode
createNode tag attrs children =
    XMLNode { tag = tag, attributes = attrs, children = children }


toXmml : String -> List Model -> Graph.Graph -> XMLNode
toXmml title allModels graph =
    let
        nodes =
            Graph.nodes graph

        models : List Model
        models =
            Graph.modelNodes graph
                |> List.map Tuple.second
                |> List.filterMap (flip findΜodelByUUID allModels)

        uniqueModels =
            models
                |> List.foldr
                    (\model list ->
                        if List.member model list then
                            list
                        else
                            model :: list
                    )
                    []

        conns =
            Graph.connections graph

        modelParamToNode : Bool -> ModelInOutput -> XMLNode
        modelParamToNode isInput { name, isDynamic, dataType } =
            let
                operator =
                    if isInput then
                        if isDynamic then
                            "S"
                        else
                            "finit"
                    else if isDynamic then
                        "Oi"
                    else
                        "Of"

                attrs =
                    [ "id" => name
                    , "operator" => operator
                    , "datatype" => dataType
                    ]

                tag =
                    if isInput then
                        "in"
                    else
                        "out"
            in
                nodeAttrs tag attrs

        uuid2ncname uuid =
            "_" ++ uuid

        modelToNode : Model -> XMLNode
        modelToNode { uuid, title, inPorts, outPorts } =
            let
                timescale =
                    nodeAttrs "timescale" [ "delta" => "1E-3", "total" => "1E-1" ]
            in
                createNode "submodel"
                    [ "id" => uuid2ncname uuid, "name" => title ]
                    [ timescale
                    , (inPorts |> List.map (modelParamToNode True))
                        ++ (outPorts |> List.map (modelParamToNode False))
                        |> nodeChildren "ports"
                    ]

        submodels =
            List.map modelToNode uniqueModels

        instanceId : Graph.NodeId -> String
        instanceId (Graph.NodeId id) =
            "i" ++ toString id

        instances =
            nodes
                |> List.map
                    (\{ id, kind } ->
                        case kind of
                            Graph.ModelNode uuid ->
                                nodeAttrs "instance" [ ( "id", instanceId id ), ( "submodel", uuid2ncname uuid ) ]
                    )
                |> List.append
                    [ nodeAttrs "instance" [ "id" => "input", "terminal" => "input" ]
                    , nodeAttrs "instance" [ "id" => "output", "terminal" => "output" ]
                    ]

        createTerminalPort : String -> Bool -> State.ModelInOutput -> XMLNode
        createTerminalPort terminalId isInput { name, dataType } =
            let
                direction =
                    if isInput then
                        "out"
                    else
                        "in"

                portId =
                    terminalId ++ "_" ++ name
            in
                nodeAttrs direction
                    [ "id" => portId
                    , "datatype" => dataType
                    ]

        createTerminal : Bool -> List ( Graph.Node, List State.ModelInOutput ) -> XMLNode
        createTerminal isInput lst =
            let
                terminalId =
                    if isInput then
                        "input"
                    else
                        "output"

                ports =
                    List.concatMap (\( { id }, params ) -> List.map (createTerminalPort (instanceId id) isInput) params) lst |> nodeChildren "ports"
            in
                createNode "terminal"
                    [ "id" => terminalId
                    , "type"
                        => if isInput then
                            "source"
                           else
                            "sink"
                    ]
                    [ ports ]

        createTerminalCoupling : String -> Bool -> State.ModelInOutput -> XMLNode
        createTerminalCoupling instanceId isInput { name, dataType } =
            let
                terminalId =
                    if isInput then
                        "input"
                    else
                        "output"

                portId =
                    instanceId ++ "_" ++ name

                from =
                    if isInput then
                        "input" ++ "." ++ portId
                    else
                        instanceId ++ "." ++ name

                to =
                    if isInput then
                        instanceId ++ "." ++ name
                    else
                        "output" ++ "." ++ portId
            in
                nodeAttrs "coupling" [ "from" => from, "to" => to ]

        freeInputs =
            State.freeInputsOfHypermodel graph models

        freeOutputs =
            State.freeOutputsOfHypermodel graph models

        terminals =
            [ createTerminal True freeInputs
            , createTerminal False freeOutputs
            ]

        couplings =
            conns
                |> List.map
                    (\{ sourceId, sourcePort, targetId, targetPort } ->
                        nodeAttrs "coupling"
                            [ ( "from", (instanceId sourceId) ++ "." ++ sourcePort )
                            , ( "to", (instanceId targetId) ++ "." ++ targetPort )
                            ]
                    )

        terminalInCouplings =
            freeInputs
                |> List.concatMap (\( { id }, params ) -> List.map (createTerminalCoupling (instanceId id) True) params)

        terminalOutCouplings =
            freeOutputs
                |> List.concatMap (\( { id }, params ) -> List.map (createTerminalCoupling (instanceId id) False) params)

        topology =
            nodeChildren "topology" (instances ++ couplings ++ terminalInCouplings ++ terminalOutCouplings)

        datatypes =
            [ nodeAttrs "datatype" [ "id" => "number", "size_estimate" => "sizeof(double)" ] ]

        definitions =
            nodeChildren "definitions" (terminals ++ datatypes ++ submodels)
    in
        createNode "model"
            [ "id" => uuid2ncname graph.uuid
            , "name" => title
            , "xmml_version" => "0.4"
            , "xmlns" => "http://www.mapper-project.eu/xmml"
            , "xmlns:xi" => "http://www.w3.org/2001/XInclude"
            ]
            [ definitions, topology ]


{-| The characters that need to be escaped in XML docs. See
    http://stackoverflow.com/questions/1091945/what-characters-do-i-need-to-escape-in-xml-documents
-}
xmlEscapeString : String -> String
xmlEscapeString value =
    let
        charsToEscapeInXml : List ( String, String )
        charsToEscapeInXml =
            [ "&" => "&amp;"
            , "\"" => "&quot;"
            , "'" => "&apos;"
            , "<" => "&lt;"
            , ">" => "&gt;"
            ]
    in
        List.foldl (\( char, esc ) s -> String.Extra.replace char esc s)
            value
            charsToEscapeInXml


attrToString : XMLAttr -> String
attrToString ( name, value ) =
    let
        escapedValue =
            xmlEscapeString value
    in
        name ++ "=\"" ++ escapedValue ++ "\""


nodeToString : Int -> XMLNode -> String
nodeToString ident node =
    case node of
        Text str ->
            xmlEscapeString str

        XMLNode { tag, attributes, children } ->
            let
                attrs =
                    List.map attrToString attributes |> String.join " "

                content =
                    List.map (nodeToString (ident + 1)) children |> String.join "\n"

                prefix =
                    String.repeat ident "  "

                startTag =
                    prefix ++ "<" ++ tag ++ " " ++ attrs ++ ">\n"

                endTag =
                    prefix ++ "</" ++ tag ++ ">"
            in
                if List.isEmpty children then
                    prefix ++ "<" ++ tag ++ " " ++ attrs ++ "/>"
                else
                    startTag ++ content ++ "\n" ++ endTag


toXmmlString : String -> List Model -> Graph.Graph -> String
toXmmlString title allModels graph =
    toXmml title allModels graph |> nodeToString 0 |> (++) "<?xml version=\"1.0\"?>\n"
