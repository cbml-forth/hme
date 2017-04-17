module State exposing (..)

import AllDict
import Date exposing (Date)
import Dict
import Set
import Either exposing (Either(..), lefts, rights)
import Graph
import Http
import List.Extra
import Number.Expanded exposing (..)
import Random.Pcg exposing (Seed, initialSeed, step)
import RemoteData exposing (WebData)
import Utils exposing (..)
import Uuid


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
    , publishedRepoId : Maybe Int
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
    , publishedRepoId = Nothing
    }


type alias ValueRange =
    ( Expanded Float, Expanded Float )


type alias ModelInOutput =
    { repoId : Int
    , uuid : String
    , name : String
    , isDynamic : Bool
    , isMandatory : Bool
    , dataType : String
    , units : String
    , description : String
    , range : Maybe ValueRange
    , defaultValue : Maybe String
    , semtype : List String
    }


type ModelInputWithValue
    = ModelInputWithValue ModelInOutput String


type alias Perspective =
    { index : Int, name : String, uri : String, values : List ( String, String ) }


perspective1 : Perspective
perspective1 =
    Perspective 1
        "Tumor-affected normal tissue modelling"
        "http://www.chic-vph.eu/ontologies/resource#hasPositionIn-1"
        [ "http://purl.obolibrary.org/obo/HP_0002664" => "Tumor"
        , "http://purl.obolibrary.org/obo/HP_0000969" => "Oedima"
        ]


perspective2 : Perspective
perspective2 =
    Perspective 2
        "Spatial scales"
        "http://www.chic-vph.eu/ontologies/resource#hasPositionIn-2"
        [ "http://www.chic-vph.eu/ontologies#chic_0000201" => "Atomic"
        , "http://www.chic-vph.eu/ontologies#chic_0000202" => "Molecular"
        , "http://www.chic-vph.eu/ontologies#chic_0000203" => "Cellular"
        , "http://www.chic-vph.eu/ontologies#chic_0000204" => "Tissue"
        , "http://www.chic-vph.eu/ontologies#chic_0000205" => "Organ"
        , "http://www.chic-vph.eu/ontologies#chic_0000206" => "Body system"
        , "http://www.chic-vph.eu/ontologies#chic_0000207" => "Organism"
        , "http://www.chic-vph.eu/ontologies#chic_0000208" => "Population"
        ]


perspective4 : Perspective
perspective4 =
    Perspective 4
        "Biomechanism(s) addressed"
        "http://www.chic-vph.eu/ontologies/resource#hasPositionIn-4"
        [ "http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#C19323" => "Basic tumour biology"
        , "http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#C16346" => "Biomechanics"
        , "http://purl.obolibrary.org/obo/GO_0003674" => "Molecular"
        , "http://purl.obolibrary.org/obo/GO_0008150" => "Angiogenesis"
        , "http://purl.obolibrary.org/obo/GO_0008152" => "Metabolism"
        , "http://purl.obolibrary.org/obo/GO_0022402" => "Cell cycle"
        , "http://purl.obolibrary.org/obo/GO_0070265" => "Necrosis"
        ]


perspective5 : Perspective
perspective5 =
    Perspective 5
        "Tumour type(s) addressed"
        "http://www.chic-vph.eu/ontologies/resource#hasPositionIn-5"
        [ "http://purl.obolibrary.org/obo/HP_0100526" => "Lung cancer"
        , "http://purl.obolibrary.org/obo/HP_0002667" => "Wilm's tumor"
        , "http://purl.obolibrary.org/obo/HP_0003003" => "Colon cancer"
        , "http://purl.obolibrary.org/obo/HP_0012125" => "Prostate cancer"
        , "http://purl.obolibrary.org/obo/HP_0100843" => "Glioblastoma"
        ]


perspective8 : Perspective
perspective8 =
    Perspective 8
        "Order of Addressing Spatial scales"
        "http://www.chic-vph.eu/ontologies/resource#hasPositionIn-8"
        [ "http://www.chic-vph.eu/ontologies#chic_0000211" => "Bottom-Up"
        , "http://www.chic-vph.eu/ontologies#chic_0000212" => "Middle-Out"
        , "http://www.chic-vph.eu/ontologies#chic_0000213" => "Top-Down"
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
    , usage : Int
    , isHypermodel : Bool
    }


findInputParam : Model -> String -> Maybe ModelInOutput
findInputParam { inPorts } param =
    List.Extra.find (.name >> (==) param) inPorts


findOutputParam : Model -> String -> Maybe ModelInOutput
findOutputParam { outPorts } param =
    List.Extra.find (.name >> (==) param) outPorts


type alias ModelExecutionInputs =
    Dict.Dict String String


{-| A list of pairs of node ids and the corresponding inputs the user has filled in
-}
type alias HypermodelExecutionInput =
    AllDict.AllDict Graph.NodeId ModelExecutionInputs Int


type alias ExecutionInfo =
    { inputs : HypermodelExecutionInput
    , useCaching : Set.Set Int
    }


type alias ModelSearchState =
    { title : Maybe String
    , frozenOnly : Bool
    , showStronglyCoupled : Bool
    , showNonStronglyCoupled : Bool
    , perspectives : Dict.Dict String String
    , showCompositeOnly : Bool
    }


type ModalWin
    = ListModelsWin
    | ListHypermodelsWin
    | SaveHypermodelWin
    | NodeDetailsWin
    | ErrorWin
    | InfoWin
    | XMMLWin
    | LaunchExecutionWin
    | ShowExperimentsWin


type alias ModalWinState =
    { openModals : List ModalWin
    }


type AlertError
    = HttpError Http.Error
    | OtherError (List String)
    | NoError


type alias Experiment =
    { experimentUUID : String
    , hypermodelId : String
    , experimentRepoId : Int
    , status : String
    , title : String
    , version : Int
    }


type alias Experiments =
    List Experiment


type alias State =
    { loadedHypermodel : Maybe Hypermodel
    , wip : Hypermodel
    , mml : String
    , selectedNode : Maybe Graph.NodeId
    , needsSaving : Bool
    , pendingRestCalls : Int
    , busyMessage : String
    , uuid : UUID
    , allHypermodels : WebData (List Hypermodel)
    , allModels : WebData (List Model)
    , modalsState : ModalWinState
    , zoomLevel : Float
    , modelSearch : ModelSearchState
    , executionInfo : ExecutionInfo
    , serverError : AlertError
    , infoMessage : ( String, String )
    , experiments : List Experiment
    , notificationCount : Int
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
            listFind ((==) uri << Tuple.first) values |> Maybe.map Tuple.second
    in
        Dict.get uri annotations |> Maybe.map (List.filterMap findValUri) |> Maybe.withDefault []


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
    , showStronglyCoupled = True
    , showNonStronglyCoupled = True
    , perspectives = Dict.empty
    , showCompositeOnly = False
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
        check2 modelPersps =
            Dict.map (\puri pvalue -> Dict.get puri modelPersps |> Maybe.map (\v -> List.member pvalue v) |> Maybe.withDefault False) perspectives
                |> Dict.values
                |> List.all identity
    in
        List.filter (.annotations >> check2) models


initCanvasState : State -> State
initCanvasState state =
    let
        ( uuid, state2 ) =
            newUuid state

        u =
            uuid |> Uuid.toString
    in
        { state2
            | loadedHypermodel = Nothing
            , wip =
                emptyHypermodel u
            , mml = ""
            , selectedNode = Nothing
            , needsSaving = False
            , modalsState = { openModals = [] }
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
            | allHypermodels = RemoteData.NotAsked
            , allModels = RemoteData.NotAsked
            , modelSearch = initModelSearch
        }


toggleCaching : Set.Set Int -> Graph.NodeId -> Bool -> Set.Set Int
toggleCaching intSetSet nodeIdGraph enabled =
    let
        nid =
            Graph.ordNodeId nodeIdGraph
    in
        if enabled then
            Set.insert nid intSetSet
        else
            Set.remove nid intSetSet


initExecutionInfo : ExecutionInfo
initExecutionInfo =
    { inputs = AllDict.empty Graph.ordNodeId
    , useCaching = Set.empty
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
            , allHypermodels = RemoteData.NotAsked
            , allModels = RemoteData.NotAsked
            , modalsState = { openModals = [] }
            , pendingRestCalls = 0
            , busyMessage = "Loading.."
            , zoomLevel = 1.0
            , uuid =
                { currentSeed = newSeed
                , currentUuid = newUuid
                }
            , modelSearch = initModelSearch
            , executionInfo = initExecutionInfo
            , serverError = NoError
            , infoMessage = ( "", "" )
            , experiments = []
            , notificationCount = 0
            }
    in
        ( initialState
        , Cmd.none
        )


updateModels : List Model -> State -> State
updateModels models state =
    let
        ms =
            List.sortBy .title models
    in
        { state | allModels = RemoteData.Success ms }


updateHypermodels : List Hypermodel -> State -> State
updateHypermodels hypermodels state =
    { state | allHypermodels = List.sortBy .title hypermodels |> RemoteData.Success }


findModelByUUID : List Model -> String -> Maybe Model
findModelByUUID list uuid =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if first.uuid == uuid then
                Just first
            else
                findModelByUUID rest uuid


findModel : State -> String -> Maybe Model
findModel state uuid =
    RemoteData.toMaybe state.allModels
        |> Maybe.andThen (\allModels -> findModelByUUID allModels uuid)


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
        |> Maybe.andThen (findModel state)


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


newUuid : State -> ( Uuid.Uuid, State )
newUuid state =
    let
        ( newUuid, newSeed ) =
            step Uuid.uuidGenerator state.uuid.currentSeed

        u_ =
            { currentUuid = newUuid
            , currentSeed = newSeed
            }
    in
        ( newUuid, { state | uuid = u_ } )


{-| Given an integer it returns a list of UUIDs of this size
-}
newUuids : Int -> State -> ( List Uuid.Uuid, State )
newUuids n state =
    let
        newUuidsAux : Int -> List Uuid.Uuid -> State -> ( List Uuid.Uuid, State )
        newUuidsAux n sofar state =
            case n of
                0 ->
                    ( sofar
                    , state
                    )

                _ ->
                    let
                        ( uuid, newstate ) =
                            newUuid state

                        uuids =
                            uuid :: sofar

                        remaining =
                            n - 1
                    in
                        newUuidsAux remaining uuids newstate
    in
        newUuidsAux n [] state


newHypermodel : State -> State
newHypermodel state =
    initCanvasState state


isEmptyCanvas : State -> Bool
isEmptyCanvas state =
    state.wip.graph |> Graph.nodes |> List.isEmpty |> not


usedModels : Graph.Graph -> List Model -> List ( Graph.NodeId, Model )
usedModels graph allModels =
    Graph.modelNodes graph
        |> List.filterMap (Tuple.mapSecond (findModelByUUID allModels) >> Utils.liftMaybeToTuple)


hypermodelIsStronglyCoupled : Hypermodel -> List Model -> Bool
hypermodelIsStronglyCoupled { graph } allModels =
    usedModels graph allModels |> List.map Tuple.second |> List.any modelIsDynamic


freeParamsOfHypermodel : Bool -> Graph.Graph -> List Model -> List ( Graph.Node, List ModelInOutput )
freeParamsOfHypermodel checkInputs graph listModels =
    let
        allModels : Dict.Dict String Model
        allModels =
            listModels |> List.Extra.zip (List.map .uuid listModels) |> Dict.fromList

        nodes =
            Graph.nodes graph

        partitionParams : Graph.NodeId -> List (Either String String)
        partitionParams nodeId =
            Graph.connectionsOfNode nodeId graph
                |> List.map
                    (\conn ->
                        if conn.targetId == nodeId then
                            Left conn.targetPort
                        else
                            Right conn.sourcePort
                    )

        freeParamsOf_ : Graph.NodeId -> Model -> List ModelInOutput
        freeParamsOf_ nodeId { inPorts, outPorts } =
            let
                connectedParams =
                    if checkInputs then
                        partitionParams nodeId |> lefts
                    else
                        partitionParams nodeId |> rights

                ports =
                    if checkInputs then
                        inPorts
                    else
                        outPorts
            in
                ports |> List.filter (\{ name } -> not <| List.member name connectedParams)

        freeParamsOf : Graph.Node -> Maybe ( Graph.Node, List ModelInOutput )
        freeParamsOf ({ id, kind } as node) =
            case kind of
                Graph.ModelNode uuid ->
                    Dict.get uuid allModels |> Maybe.map (freeParamsOf_ id >> (=>) node)
    in
        List.filterMap freeParamsOf nodes


freeInputsOfHypermodel : Graph.Graph -> List Model -> List ( Graph.Node, List ModelInOutput )
freeInputsOfHypermodel =
    freeParamsOfHypermodel True


freeOutputsOfHypermodel : Graph.Graph -> List Model -> List ( Graph.Node, List ModelInOutput )
freeOutputsOfHypermodel =
    freeParamsOfHypermodel False


overrideFilledInputs : String -> String -> ModelExecutionInputs -> ModelExecutionInputs
overrideFilledInputs param value =
    Dict.insert param value


emptyExecutionInputs : HypermodelExecutionInput
emptyExecutionInputs =
    AllDict.empty Graph.ordNodeId


executionInputFor : HypermodelExecutionInput -> Graph.NodeId -> String -> Maybe String
executionInputFor executionInputs nodeId paramName =
    AllDict.get nodeId executionInputs |> Maybe.andThen (Dict.get paramName)


newExperiment : Experiment -> State -> State
newExperiment experiment state =
    { state | experiments = experiment :: state.experiments }
