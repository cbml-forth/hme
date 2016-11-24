module State exposing (..)

import Uuid
import Random.Pcg exposing (Seed, initialSeed, step)
import Graph
import Http
import RemoteData exposing (WebData)
import Date exposing (Date)


type alias UUID =
    { currentSeed : Seed
    , currentUuid : Uuid.Uuid
    }


type alias Hypermodel =
    { title : String
    , id : String
    , description : String
    , version : Int
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
    , version = -1
    , canvas = ""
    , created = Date.fromTime 0
    , updated = Date.fromTime 0
    , svgContent = ""
    , graph = Graph.newGraph id
    }


type alias ModelInOutput =
    { name : String
    , is_dynamic : Bool
    }


type alias Model =
    { title : String
    , id : Int
    , uuid : String
    , description : String
    , frozen : Bool
    , inPorts : List ModelInOutput
    , outPorts : List ModelInOutput
    }


type alias ModelSearchState =
    { title : Maybe String
    , frozenOnly : Bool
    , stronglyCoupledOnly : Bool
    , perspective1 : Maybe String
    }


type alias State =
    { loadedHypermodel : Maybe Hypermodel
    , wip : Hypermodel
    , needsSaving : Bool
    , pendingRestCalls : Int
    , busyMessage : String
    , uuid :
        UUID
        -- , graph : Graph.Graph
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
            List.any .is_dynamic ports
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


initModelSearch : ModelSearchState
initModelSearch =
    { title = Nothing
    , frozenOnly = False
    , stronglyCoupledOnly = True
    , perspective1 = Nothing
    }


initializeState : State -> State
initializeState state =
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
                -- , graph = Graph.newGraph u
            , needsSaving = False
            , allHypermodels = []
            , allModels = RemoteData.NotAsked
            , showHypermodels = False
            , showModels = False
            , pendingRestCalls = 0
            , busyMessage = "Loading.."
            , zoomLevel = 1.0
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
    initializeState state
