module Xmml exposing (toXmml, toXmmlString)

import String
import List
import Graph
import State exposing (Model, ModelInOutput, findΜodelByUUID)


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


createNode : String -> List XMLAttr -> List XMLNode -> XMLNode
createNode tag attrs children =
    XMLNode { tag = tag, attributes = attrs, children = children }


toXmml : List Model -> Graph.Graph -> XMLNode
toXmml allModels graph =
    let
        nodes =
            Graph.nodes graph

        models : List Model
        models =
            List.filterMap
                (\{ kind } ->
                    case kind of
                        Graph.ModelNode uuid ->
                            findΜodelByUUID uuid allModels
                )
                nodes

        conns =
            Graph.connections graph

        modelParamToNode : String -> ModelInOutput -> XMLNode
        modelParamToNode tag { name, isDynamic } =
            let
                attrs =
                    [ ( "id", name )
                    , ( "operator"
                      , if isDynamic then
                            "Oi"
                        else
                            "S"
                      )
                    ]
            in
                createNode tag attrs []

        modelToNode : Model -> XMLNode
        modelToNode { uuid, title, inPorts, outPorts } =
            createNode "submodel"
                [ ( "id", uuid ), ( "name", title ) ]
                [ (inPorts |> List.map (modelParamToNode "in"))
                    ++ (outPorts |> List.map (modelParamToNode "out"))
                    |> nodeChildren "ports"
                ]

        nodesForModels =
            List.map modelToNode models

        descriptions =
            nodeChildren "description" [ Text "blablas" ]
    in
        createNode "model"
            [ ( "id", graph.uuid )
            , ( "name", "Nephroblastoma_muscle_multimodeller_hypermodel" )
            , ( "xmml_version", "0.4" )
            , ( "xmlns", "http://www.mapper-project.eu/xmml" )
            , ( "xmlns:xi", "http://www.w3.org/2001/XInclude" )
            ]
            nodesForModels


attrToString : XMLAttr -> String
attrToString ( name, value ) =
    name ++ "=\"" ++ value ++ "\""


nodeToString : XMLNode -> String
nodeToString node =
    case node of
        Text str ->
            str

        XMLNode { tag, attributes, children } ->
            let
                attrs =
                    List.map attrToString attributes |> String.join " "

                content =
                    List.map nodeToString children |> String.join "\n\t"

                startTag =
                    "<" ++ tag ++ " " ++ attrs ++ ">"

                endTag =
                    "</" ++ tag ++ ">"
            in
                if List.isEmpty children then
                    "<" ++ tag ++ " " ++ attrs ++ "/>"
                else
                    startTag ++ content ++ "\n" ++ endTag


toXmmlString : List Model -> Graph.Graph -> String
toXmmlString allModels graph =
    toXmml allModels graph |> nodeToString
