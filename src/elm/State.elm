module State exposing (..)

import Uuid
import Random.Pcg exposing (Seed, initialSeed, step)
import Graph
import Http
import RemoteData exposing (WebData)
import Date exposing (Date)
import Number.Expanded exposing (..)
import Dict
import List.Extra


type alias UUID =
    { currentSeed : Seed
    , currentUuid : Uuid.Uuid
    }


type alias Hypermodel =
    { title : String
    , id : String
    , description : String
    , version : String
    , canvas : String
    , created : Date
    , updated : Date
    , svgContent : String
    , graph : Graph.Graph
    }


emptyHypermodel : String -> Hypermodel
emptyHypermodel id =
    { title = ""
    , id = id
    , description = ""
    , version = ""
    , canvas = ""
    , created = Date.fromTime 0
    , updated = Date.fromTime 0
    , svgContent = ""
    , graph = Graph.new id
    }


type alias ValueRange =
    ( Expanded Float, Expanded Float )


type alias ModelInOutput =
    { name : String
    , isDynamic : Bool
    , dataType : String
    , units : String
    , description : String
    , range : Maybe ValueRange
    , defaultValue : Maybe String
    }


type alias Perspective =
    { index : Int, name : String, uri : String, values : List ( String, String ) }


perspective1 : Perspective
perspective1 =
    Perspective 1
        "Tumor-affected normal tissue modelling"
        "http://www.chic-vph.eu/ontologies/resource#hasPositionIn-1"
        [ ( "http://purl.obolibrary.org/obo/HP_0002664", "Tumor" )
        , ( "http://purl.obolibrary.org/obo/HP_0000969", "Oedima" )
        ]


perspective2 : Perspective
perspective2 =
    Perspective 2
        "Spatial scales"
        "http://www.chic-vph.eu/ontologies/resource#hasPositionIn-2"
        [ ( "http://www.chic-vph.eu/ontologies#chic_0000201", "Atomic" )
        , ( "http://www.chic-vph.eu/ontologies#chic_0000202", "Molecular" )
        , ( "http://www.chic-vph.eu/ontologies#chic_0000203", "Cellular" )
        , ( "http://www.chic-vph.eu/ontologies#chic_0000204", "Tissue" )
        , ( "http://www.chic-vph.eu/ontologies#chic_0000205", "Organ" )
        , ( "http://www.chic-vph.eu/ontologies#chic_0000206", "Body system" )
        , ( "http://www.chic-vph.eu/ontologies#chic_0000207", "Organism" )
        , ( "http://www.chic-vph.eu/ontologies#chic_0000208", "Population" )
        ]


perspective4 : Perspective
perspective4 =
    Perspective 4
        "Biomechanism(s) addressed"
        "http://www.chic-vph.eu/ontologies/resource#hasPositionIn-4"
        [ ( "http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#C19323", "Basic tumour biology" )
        , ( "http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#C16346", "Biomechanics" )
        , ( "http://purl.obolibrary.org/obo/GO_0003674", "Molecular" )
        , ( "http://purl.obolibrary.org/obo/GO_0008150", "Angiogenesis" )
        , ( "http://purl.obolibrary.org/obo/GO_0008152", "Metabolism" )
        , ( "http://purl.obolibrary.org/obo/GO_0022402", "Cell cycle" )
        , ( "http://purl.obolibrary.org/obo/GO_0070265", "Necrosis" )
        ]


perspective5 : Perspective
perspective5 =
    Perspective 5
        "Tumour type(s) addressed"
        "http://www.chic-vph.eu/ontologies/resource#hasPositionIn-5"
        [ ( "http://purl.obolibrary.org/obo/HP_0100526", "Lung cancer" )
        , ( "http://purl.obolibrary.org/obo/HP_0002667", "Wilm's tumor" )
        , ( "http://purl.obolibrary.org/obo/HP_0003003", "Colon cancer" )
        , ( "http://purl.obolibrary.org/obo/HP_0012125", "Prostate cancer" )
        , ( "http://purl.obolibrary.org/obo/HP_0100843", "Glioblastoma" )
        ]


perspective8 : Perspective
perspective8 =
    Perspective 8
        "Order of Addressing Spatial scales"
        "http://www.chic-vph.eu/ontologies/resource#hasPositionIn-8"
        [ ( "http://www.chic-vph.eu/ontologies#chic_0000211", "Bottom-Up" )
        , ( "http://www.chic-vph.eu/ontologies#chic_0000212", "Middle-Out" )
        , ( "http://www.chic-vph.eu/ontologies#chic_0000213", "Top-Down" )
        ]


perspectives : List Perspective
perspectives =
    [ perspective1
    , perspective2
    , perspective4
    , perspective5
    , perspective8
    ]


type alias Model =
    { title : String
    , id : Int
    , uuid : String
    , description : String
    , frozen : Bool
    , inPorts : List ModelInOutput
    , outPorts : List ModelInOutput
    , annotations :
        Dict.Dict String (List String)
        -- List ( String, String )
        -- For the perspectives annotations
    , usage : Int
    }


type alias ModelSearchState =
    { title : Maybe String
    , frozenOnly : Bool
    , stronglyCoupledOnly : Bool
    , perspectives : Dict.Dict String String
    }


type alias State =
    { loadedHypermodel : Maybe Hypermodel
    , wip : Hypermodel
    , mml : String
    , selectedNode : Maybe String
    , needsSaving : Bool
    , pendingRestCalls : Int
    , busyMessage : String
    , uuid : UUID
    , allHypermodels : List Hypermodel
    , allModels : WebData (List Model)
    , showHypermodels : Bool
    , showModels : Bool
    , zoomLevel : Float
    , modelSearch : ModelSearchState
    , serverError : Maybe Http.Error
    }


modelIsDynamic : Model -> Bool
modelIsDynamic model =
    let
        hasDynamicPort ports =
            List.any .isDynamic ports
    in
        hasDynamicPort model.inPorts || hasDynamicPort model.outPorts


modelIsUsed : State -> Model -> Bool
modelIsUsed state model =
    let
        nodes =
            Graph.nodes state.wip.graph

        modelId =
            model.uuid
    in
        List.any
            (\n ->
                case n.kind of
                    Graph.ModelNode u ->
                        u == modelId
            )
            nodes


perspValueForModel : Model -> Perspective -> List String
perspValueForModel { annotations } { uri, values } =
    let
        findValUri : String -> Maybe String
        findValUri uri =
            List.filterMap
                (\( u, v ) ->
                    if u == uri then
                        Just v
                    else
                        Nothing
                )
                values
                |> List.head
    in
        Dict.get uri annotations |> Maybe.map (List.filterMap findValUri) |> Maybe.withDefault []



-- Use the Perspectives to get a list of string annotations


tagsForModel : Model -> List String
tagsForModel model =
    List.concatMap (perspValueForModel model) perspectives


tagsForHyperModel : List Model -> Hypermodel -> List String
tagsForHyperModel allModels hypermodel =
    let
        models =
            modelsOfHypermodel allModels hypermodel

        persps =
            [ perspective1, perspective4 ]
    in
        List.concatMap (\m -> List.concatMap (perspValueForModel m) persps) models
            |> List.Extra.unique


modelsOfHypermodel : List Model -> Hypermodel -> List Model
modelsOfHypermodel allModels { graph } =
    let
        ids =
            Graph.nodes graph |> List.map .kind |> List.map (\(Graph.ModelNode s) -> s)
    in
        allModels |> List.filter (\{ uuid } -> List.member uuid ids)


initModelSearch : ModelSearchState
initModelSearch =
    { title = Nothing
    , frozenOnly = False
    , stronglyCoupledOnly = True
    , perspectives = Dict.empty
    }


updateModelSearchPersp : ModelSearchState -> String -> Maybe String -> ModelSearchState
updateModelSearchPersp ({ perspectives } as state) uri maybeValue =
    let
        persp =
            case maybeValue of
                Nothing ->
                    Dict.remove uri perspectives

                Just s ->
                    Dict.insert uri s perspectives
    in
        { state | perspectives = persp }


filterModelsByPerspective : ModelSearchState -> List Model -> List Model
filterModelsByPerspective { perspectives } models =
    let
        -- check ( puri, pvalue ) =
        --     case Dict.get puri perspectives of
        --         Nothing ->
        --             True
        --
        --         Just v ->
        --             pvalue == v
        check2 modelPersps =
            Dict.map (\puri pvalue -> Dict.get puri modelPersps |> Maybe.map (\v -> List.member pvalue v) |> Maybe.withDefault False) perspectives
                |> Dict.values
                |> List.all identity
    in
        List.filter (.annotations >> check2) models


initCanvasState : State -> State
initCanvasState state =
    let
        state2 =
            newUuid state

        u =
            state2 |> .uuid |> .currentUuid |> Uuid.toString
    in
        { state2
            | loadedHypermodel = Nothing
            , wip =
                emptyHypermodel u
            , mml = ""
            , selectedNode = Nothing
            , needsSaving = False
            , showHypermodels = False
            , showModels = False
            , busyMessage = "Loading.."
            , zoomLevel = 1.0
        }


initializeState : State -> State
initializeState state =
    let
        state2 =
            initCanvasState state
    in
        { state2
            | allHypermodels = []
            , allModels = RemoteData.NotAsked
            , modelSearch = initModelSearch
        }


init : Int -> ( State, Cmd a )
init seed =
    let
        ( newUuid, newSeed ) =
            step Uuid.uuidGenerator (initialSeed seed)

        u =
            Uuid.toString newUuid

        initialState =
            { loadedHypermodel = Nothing
            , wip = emptyHypermodel u
            , mml = ""
            , selectedNode = Nothing
            , needsSaving = False
            , allHypermodels = []
            , allModels = RemoteData.NotAsked
            , showHypermodels = False
            , showModels = False
            , pendingRestCalls = 0
            , busyMessage = "Loading.."
            , zoomLevel = 1.0
            , uuid =
                { currentSeed = newSeed
                , currentUuid = newUuid
                }
            , modelSearch = initModelSearch
            , serverError = Nothing
            }
    in
        ( initialState
        , Cmd.none
        )


updateHypermodels : List Hypermodel -> State -> State
updateHypermodels hypermodels state =
    { state | allHypermodels = List.sortBy .title hypermodels }


findΜodelByUUID : String -> List Model -> Maybe Model
findΜodelByUUID uuid list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if first.uuid == uuid then
                Just first
            else
                findΜodelByUUID uuid rest


findΜodel : State -> String -> Maybe Model
findΜodel state uuid =
    RemoteData.toMaybe state.allModels |> Maybe.andThen (findΜodelByUUID uuid)


findSelectedModel : State -> Maybe Model
findSelectedModel state =
    state.selectedNode
        |> Maybe.andThen (Graph.findNode state.wip.graph)
        |> Maybe.map
            (\n ->
                case n.kind of
                    Graph.ModelNode modelId ->
                        modelId
            )
        |> Maybe.andThen (findΜodel state)


findHypermodelByUUID : String -> List Hypermodel -> Maybe Hypermodel
findHypermodelByUUID uuid list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if first.id == uuid then
                Just first
            else
                findHypermodelByUUID uuid rest


newUuid : State -> State
newUuid state =
    let
        ( newUuid, newSeed ) =
            step Uuid.uuidGenerator state.uuid.currentSeed

        u_ =
            { currentUuid = newUuid
            , currentSeed = newSeed
            }
    in
        { state | uuid = u_ }


newHypermodel : State -> State
newHypermodel state =
    initCanvasState state


isEmptyCanvas : State -> Bool
isEmptyCanvas state =
    state.wip.graph |> Graph.nodes |> List.isEmpty |> not
