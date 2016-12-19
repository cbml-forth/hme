module Xmml exposing (toXmml, toXmmlString)

import String
import List
import Graph
import State exposing (Model, ModelInOutput, findΜodelByUUID)
import String.Extra


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
            nodes
                |> List.filterMap
                    (\{ kind } ->
                        case kind of
                            Graph.ModelNode uuid ->
                                findΜodelByUUID uuid allModels
                    )

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
                    [ ( "id", name )
                    , ( "operator", operator )
                    , ( "datatype", dataType )
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
                    nodeAttrs "timescale" [ ( "delta", "1E-3" ), ( "total", "1E-1" ) ]
            in
                createNode "submodel"
                    [ ( "id", uuid2ncname uuid ), ( "name", title ) ]
                    [ timescale
                    , (inPorts |> List.map (modelParamToNode True))
                        ++ (outPorts |> List.map (modelParamToNode False))
                        |> nodeChildren "ports"
                    ]

        submodels =
            List.map modelToNode uniqueModels

        instances =
            nodes
                |> List.map
                    (\{ id, kind } ->
                        case kind of
                            Graph.ModelNode uuid ->
                                nodeAttrs "instance" [ ( "id", "i" ++ id ), ( "submodel", uuid2ncname uuid ) ]
                    )

        couplings =
            conns
                |> List.map
                    (\{ sourceId, sourcePort, targetId, targetPort } ->
                        nodeAttrs "coupling"
                            [ ( "from", "i" ++ sourceId ++ "." ++ sourcePort )
                            , ( "to", "i" ++ targetId ++ "." ++ targetPort )
                            ]
                    )

        topology =
            nodeChildren "topology" (instances ++ couplings)

        datatypes =
            [ nodeAttrs "datatype" [ ( "id", "number" ), ( "size_estimate", "sizeof(double)" ) ] ]

        definitions =
            nodeChildren "definitions" (datatypes ++ submodels)
    in
        createNode "model"
            [ ( "id", uuid2ncname graph.uuid )
            , ( "name", title )
            , ( "xmml_version", "0.4" )
            , ( "xmlns", "http://www.mapper-project.eu/xmml" )
            , ( "xmlns:xi", "http://www.w3.org/2001/XInclude" )
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
            [ ( "&", "&amp;" )
            , ( "\"", "&quot;" )
            , ( "'", "&apos;" )
            , ( "<", "&lt;" )
            , ( ">", "&gt;" )
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
