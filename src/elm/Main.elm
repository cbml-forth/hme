module Main exposing (..)

import AllDict
import Dict
import Graph exposing (nodeIdDecoder)
import Html exposing (Html)
import Json.Encode as Encode
import Msg exposing (..)
import Navigation
import Ports exposing (addModelToGraph, loadHypermodel, loadHypermodel2, scaleGraph, serializeGraph, showOrHideModal)
import RemoteData
import Rest exposing (..)
import Return exposing ((>>>))
import State exposing (State, updateHypermodels, findHypermodelByUUID, findΜodelByUUID)
import UrlParser
import Utils exposing ((=>))
import View exposing (modalWinIds, view)
import Xmml
import UrlParser
import Utils exposing ((=>))
import View exposing (modalWinIds, view)
import Xmml
import Rest exposing (..)
import Ports
    exposing
        ( showOrHideModal
        , loadHypermodel
        , loadHypermodel2
        , addModelToGraph
        , serializeGraph
        , scaleGraph
        )
import Graph
import Msg exposing (..)
import View exposing (view, modalWinIds)
import Return exposing ((>>>))
import RemoteData
import Json.Encode as Encode
import Xmml
import Utils exposing ((=>))


subscriptions : State -> Sub Msg.Msg
subscriptions model =
    Ports.subscriptions model |> Sub.map UIMsg


debugView : State -> Html Msg.Msg
debugView state =
    Debug.log "New State:" state |> View.view


loadHypermodel : State.Hypermodel -> List State.Model -> Cmd msg
loadHypermodel hm allModels =
    let
        nodes =
            Graph.nodes hm.graph

        modelToJson : Graph.NodeId -> Graph.Position -> State.Model -> Encode.Value
        modelToJson (Graph.NodeId nodeId) pos model =
            let
                inPorts =
                    List.map (Encode.string << .name) model.inPorts |> Encode.list

                outPorts =
                    List.map (Encode.string << .name) model.outPorts |> Encode.list

                dynPorts =
                    model.inPorts
                        ++ model.outPorts
                        |> List.filter .isDynamic
                        |> List.map (.name >> Encode.string)
                        |> Encode.list

                position =
                    Encode.object [ "x" => Encode.int pos.x, "y" => Encode.int pos.y ]
            in
                Encode.object
                    [ "id" => (toString nodeId |> Encode.string)
                    , "name" => Encode.string model.title
                    , "ports"
                        => Encode.object
                            [ "inPorts" => inPorts
                            , "outPorts" => outPorts
                            , "dynPorts" => dynPorts
                            ]
                    , "position" => position
                    ]

        nodeToJson : Graph.Node -> Maybe Encode.Value
        nodeToJson { id, kind, position } =
            case kind of
                Graph.ModelNode uuid ->
                    findΜodelByUUID uuid allModels |> Maybe.map (modelToJson id position)

        connectionToJson : Graph.Connection -> Encode.Value
        connectionToJson { id, sourceId, sourcePort, targetId, targetPort, vertices } =
            Encode.object
                [ "id" => Encode.string id
                , "sourceId" => Graph.nodeIdStrEncode sourceId
                , "sourcePort" => Encode.string sourcePort
                , "targetId" => Graph.nodeIdStrEncode targetId
                , "targetPort" => Encode.string targetPort
                , "vertices" => Encode.list (List.map Graph.encodePosition vertices)
                ]

        nodesListJson =
            List.filterMap nodeToJson (Graph.nodes hm.graph)

        connsListJson =
            List.map connectionToJson (Graph.connections hm.graph)
    in
        Ports.loadHypermodel2
            (Encode.object
                [ "nodes" => Encode.list nodesListJson
                , "links" => Encode.list connsListJson
                ]
            )


doLoadHypermodels : (Rest.Msg Rest.HyperModels -> Msg.Msg) -> State -> ( State, Cmd Msg.Msg )
doLoadHypermodels tagger state =
    { state
        | pendingRestCalls = state.pendingRestCalls + 1
        , busyMessage = "Retrieving hypermodels.."
    }
        ! [ Cmd.map tagger Rest.getHyperModels
          ]


doLoadModels : State -> ( State, Cmd Msg.Msg )
doLoadModels state =
    { state
        | pendingRestCalls = state.pendingRestCalls + 1
        , busyMessage = "Retrieving models.."
    }
        ! [ Cmd.map ModelsResponse Rest.getModels
          ]


doLoadAllModels : String -> State -> ( State, Cmd Msg.Msg )
doLoadAllModels hypermodelUuid state =
    { state
        | pendingRestCalls = state.pendingRestCalls + 1
        , busyMessage = "Retrieving models and hypermodels.."
    }
        ! [ Cmd.map (StateInitResponse hypermodelUuid) Rest.getAllModels
          ]


serverUpdate : Rest.Msg a -> State.State -> ( State.State, Cmd Msg.Msg )
serverUpdate response state =
    let
        calls =
            state.pendingRestCalls - 1

        newState =
            { state
                | pendingRestCalls = calls
                , serverError = Nothing
                , busyMessage =
                    if calls == 0 then
                        ""
                    else
                        state.busyMessage
            }
    in
        case response of
            Err httpError ->
                { newState | serverError = Just httpError }
                    ! [ showOrHideModal True modalWinIds.errorAlert ]

            Ok success ->
                newState ! []


startNewHypermodel : State -> ( State, Cmd Msg.Msg )
startNewHypermodel state =
    let
        newState =
            State.newHypermodel state

        allModels =
            RemoteData.withDefault [] newState.allModels
    in
        newState ! [ loadHypermodel newState.wip allModels ]


showLoadedModels : State -> ( State, Cmd Msg.Msg )
showLoadedModels state =
    case state.allModels of
        RemoteData.Success list ->
            state ! [ showOrHideModal True modalWinIds.listModels ]

        RemoteData.NotAsked ->
            { state | allModels = RemoteData.Loading } ! [ Rest.getModels |> Cmd.map ModelsResponse ]

        _ ->
            state ! []


updateHypermodels : Rest.Msg Rest.HyperModels -> State -> State
updateHypermodels response state =
    case response of
        Ok (Rest.HyperModels list) ->
            State.updateHypermodels list state

        _ ->
            state


updateModels : Rest.Msg Rest.Models -> State -> State
updateModels response state =
    case response of
        Ok (Rest.Models models) ->
            State.updateModels models state

        _ ->
            state


{-| Allows you to conditionally trigger updates based on a predicate.
-}
filterUpdate : Bool -> (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
filterUpdate pred update =
    if pred then
        Return.andThen update
    else
        Basics.identity


filterResponseUpdate_ : Rest.Msg a -> (a -> model -> ( model, Cmd msg )) -> model -> ( model, Cmd msg )
filterResponseUpdate_ response update =
    case response of
        Ok value ->
            update value

        _ ->
            Return.singleton


filterResponseUpdate : Rest.Msg a -> (a -> State -> ( State, Cmd Msg.Msg )) -> State -> ( State, Cmd Msg.Msg )
filterResponseUpdate response update =
    filterResponseUpdate_ response update >>> serverUpdate response


isOk : Result error value -> Bool
isOk r =
    case r of
        Ok _ ->
            True

        _ ->
            False


doLoadHypermodel : String -> State -> ( State, Cmd Msg.Msg )
doLoadHypermodel uuid state =
    let
        hm =
            State.findHypermodelByUUID uuid state.allHypermodels

        allModels =
            RemoteData.withDefault [] state.allModels

        newWip =
            case hm of
                Just hypermodel ->
                    hypermodel

                Nothing ->
                    state.wip
    in
        { state
            | needsSaving = False
            , wip = newWip
            , loadedHypermodel = hm
        }
            ! [ loadHypermodel newWip allModels
              ]


updateFromUI : Ports.Msg -> State -> ( State, Cmd Msg.Msg )
updateFromUI uiMsg state =
    case uiMsg of
        Ports.NewGraph { canvas, svg } ->
            let
                wip =
                    state.wip

                newWip =
                    { wip | canvas = canvas, svgContent = svg }

                newState =
                    { state
                        | pendingRestCalls = state.pendingRestCalls + 1
                        , needsSaving = True
                        , wip = newWip
                        , busyMessage = "Saving hypermodel.."
                        , zoomLevel = 1.0
                    }
            in
                newState
                    ! [ Cmd.map HypermodelSaveResponse (Rest.saveHyperModel newWip)
                      ]

        Ports.NewConnection conn ->
            let
                wip =
                    state.wip

                newGraph =
                    Graph.addConnection conn wip.graph

                needsSaving =
                    state.needsSaving || state.wip.graph /= newGraph

                newWip =
                    { wip | graph = newGraph }
            in
                { state | needsSaving = needsSaving, wip = newWip } ! []

        Ports.MoveNode nodeId position ->
            let
                wip =
                    state.wip

                graphNodeId =
                    String.toInt nodeId |> Result.withDefault 0 |> Graph.NodeId

                newGraph =
                    Graph.moveNode graphNodeId position wip.graph

                -- |> Debug.log "GRAPH = "
                newWip =
                    { wip | graph = newGraph }

                needsSaving =
                    state.needsSaving || state.wip.graph /= newGraph
            in
                { state | needsSaving = needsSaving, wip = newWip } ! []

        Ports.RemoveNode nodeId ->
            let
                wip =
                    state.wip

                graphNodeId =
                    String.toInt nodeId |> Result.withDefault 0 |> Graph.NodeId

                newGraph =
                    Graph.removeNode graphNodeId wip.graph

                newWip =
                    { wip | graph = newGraph }

                needsSaving =
                    state.needsSaving || state.wip.graph /= newGraph
            in
                { state | needsSaving = needsSaving, wip = newWip } ! []

        Ports.ShowNode nodeId ->
            let
                graphNodeId =
                    String.toInt nodeId |> Result.toMaybe |> Maybe.map Graph.NodeId
            in
                { state | selectedNode = graphNodeId } ! [ showOrHideModal True modalWinIds.showNodeModel ]

        Ports.RemoveConnection connId ->
            let
                wip =
                    state.wip

                newGraph =
                    Graph.removeConnection connId wip.graph

                -- |> Debug.log "GRAPH = "
                newWip =
                    { wip | graph = newGraph }

                needsSaving =
                    state.needsSaving || state.wip.graph /= newGraph
            in
                { state | needsSaving = needsSaving, wip = newWip } ! []


updateModelSearch : Msg.ModelSearchMsg -> State -> ( State, Cmd Msg.Msg )
updateModelSearch modelSearchMsg state =
    let
        search =
            state.modelSearch
    in
        case modelSearchMsg of
            ModelSearchTitle str ->
                let
                    search_ =
                        { search | title = Just str }
                in
                    { state | modelSearch = search_ } ! [ showOrHideModal True modalWinIds.listModels ]

            ModelSearchFrozen b ->
                let
                    search_ =
                        { search | frozenOnly = b }
                in
                    { state | modelSearch = search_ } ! [ showOrHideModal True modalWinIds.listModels ]

            ModelSearchStronglyCoupled b ->
                let
                    search_ =
                        { search | stronglyCoupledOnly = b }
                in
                    { state | modelSearch = search_ } ! [ showOrHideModal True modalWinIds.listModels ]

            ModelSearchPerspective { uri, value } ->
                { state | modelSearch = State.updateModelSearchPersp state.modelSearch uri value } ! []

            ClearSearch ->
                { state | modelSearch = State.initModelSearch } ! []


updateExecutionInputs : Msg.ExecutionInputsMsg -> State -> ( State, Cmd Msg.Msg )
updateExecutionInputs executionInputsMsgMsg state =
    let
        usedModels : List ( Graph.NodeId, State.Model )
        usedModels =
            state.allModels
                |> RemoteData.withDefault []
                |> State.usedModels state.wip.graph

        fillDefaultInputs : State.Model -> State.ModelExecutionInputs
        fillDefaultInputs { inPorts } =
            List.filterMap (Utils.on (,) .name .defaultValue >> Utils.liftMaybeToTuple) inPorts
                |> Dict.fromList

        updateWithDefaultInputs : State.Model -> Maybe State.ModelExecutionInputs -> State.ModelExecutionInputs
        updateWithDefaultInputs model previousInputs =
            let
                defInputs =
                    fillDefaultInputs model
            in
                Maybe.map (Dict.union defInputs) previousInputs |> Maybe.withDefault defInputs
    in
        case executionInputsMsgMsg of
            DoFillDefaultInputs ->
                let
                    newExc : State.HypermodelExecutionInput
                    newExc =
                        List.foldl
                            (\( nodeId, model ) newD ->
                                AllDict.update nodeId (Just << updateWithDefaultInputs model) newD
                            )
                            state.executionInputs
                            usedModels
                in
                    { state | executionInputs = newExc } ! []

            DoFillDefaultInputsOf nodeId ->
                let
                    maybeModel : Maybe State.Model
                    maybeModel =
                        Utils.listFind ((==) nodeId << Tuple.first) usedModels
                            |> Maybe.map Tuple.second

                    newExc =
                        case maybeModel of
                            Nothing ->
                                state.executionInputs

                            Just model ->
                                AllDict.update nodeId (Just << updateWithDefaultInputs model) state.executionInputs
                in
                    { state | executionInputs = newExc } ! []

            ShowFillInputsDialog ->
                state ! [ showOrHideModal True modalWinIds.fillInputsRunWin ]

            FilledInput nodeId param value ->
                let
                    newExc =
                        AllDict.update nodeId
                            (Maybe.map (State.overrideFilledInputs param value)
                                >> Maybe.withDefault (Dict.singleton param value)
                                >> Just
                            )
                            state.executionInputs
                in
                    { state | executionInputs = newExc } ! []


update : Msg.Msg -> State -> ( State, Cmd Msg.Msg )
update m state =
    case Debug.log "MSG:" m of
        LoadPage loc ->
            let
                uuid =
                    UrlParser.parseHash UrlParser.string loc |> Maybe.withDefault ""
            in
                doLoadHypermodels (OpenHypermodelResponse uuid) state

        NewHypermodel ->
            startNewHypermodel state

        LoadHypermodels ->
            doLoadHypermodels HyperModelsResponse state

        LoadModels ->
            showLoadedModels state

        Refresh ->
            doLoadHypermodels RefreshResponse state

        HyperModelsResponse response ->
            state
                |> filterResponseUpdate response
                    (\(Rest.HyperModels list) state ->
                        State.updateHypermodels list state
                            |> (\state ->
                                    { state | showHypermodels = True } ! [ showOrHideModal True modalWinIds.listHypermodels ]
                               )
                    )

        StateInitResponse uuid response ->
            state
                |> filterResponseUpdate response
                    (\(Rest.HypoHyperModels (Rest.Models models) (Rest.HyperModels hypermodels)) state ->
                        State.updateHypermodels hypermodels state
                            |> State.updateModels models
                            |> doLoadHypermodel uuid
                    )

        RefreshResponse response ->
            let
                uuid =
                    state.loadedHypermodel |> Maybe.map .id |> Maybe.withDefault ""
            in
                serverUpdate response state
                    |> Return.map (updateHypermodels response)
                    |> filterUpdate (isOk response) (doLoadHypermodel uuid)

        ModelsResponse response ->
            serverUpdate response state
                |> Return.map (updateModels response)

        HypermodelSaveResponse response ->
            state
                |> filterResponseUpdate response
                    (\(Rest.Version hypermodelUuid version) state ->
                        let
                            wip =
                                state.wip

                            newWip =
                                { wip | version = version }

                            newState =
                                if wip.id == hypermodelUuid then
                                    { state | wip = newWip, needsSaving = False }
                                else
                                    state
                        in
                            newState ! []
                    )

        CloseModal modalId ->
            { state | showModels = False, showHypermodels = False } ! [ showOrHideModal False modalId ]

        OpenHypermodel uuid ->
            doLoadHypermodel uuid state
                |> Return.command (showOrHideModal False modalWinIds.listHypermodels)

        OpenHypermodelResponse uuid response ->
            serverUpdate response state
                |> Return.map (updateHypermodels response)
                |> filterUpdate (isOk response) (doLoadHypermodel uuid)

        ReloadHypermodel ->
            let
                uuid =
                    state.loadedHypermodel |> Maybe.map .id |> Maybe.withDefault ""
            in
                doLoadHypermodel uuid state

        AddModel model ->
            let
                wip =
                    state.wip

                ( nodeId, newGraph ) =
                    Graph.newNodeId wip.graph

                pos =
                    { x = 150, y = 50 }

                node =
                    let
                        inPorts =
                            List.map .name model.inPorts

                        outPorts =
                            List.map .name model.outPorts
                    in
                        { id = nodeId
                        , inPorts = inPorts
                        , outPorts = outPorts
                        , position = pos
                        , kind = Graph.ModelNode model.uuid
                        }

                newGraph2 =
                    Graph.addNode node newGraph

                newWip =
                    { wip | graph = newGraph2 }
            in
                { state | wip = newWip, needsSaving = True }
                    ! [ showOrHideModal False modalWinIds.listModels
                      , Ports.addModelToGraph nodeId pos model
                      ]

        Export ->
            let
                wip =
                    state.wip

                mml =
                    case state.allModels of
                        RemoteData.Success models ->
                            wip.graph |> Xmml.toXmmlString wip.title models

                        _ ->
                            ""
            in
                { state | mml = mml } ! [ showOrHideModal True modalWinIds.mmlDescription ]

        SaveHypermodel ->
            state ! [ showOrHideModal True modalWinIds.saveHypermodel ]

        DoSaveHypermodel ->
            let
                newState =
                    { state
                        | pendingRestCalls = state.pendingRestCalls + 1
                        , busyMessage = "Saving hypermodel.."
                    }
            in
                state
                    ! [ showOrHideModal False modalWinIds.saveHypermodel
                      , Ports.serializeGraph ()
                      ]

        ChangeTitle str ->
            let
                wip =
                    state.wip

                newWip =
                    { wip | title = str }
            in
                { state | needsSaving = True, wip = newWip } ! []

        ChangeDescription str ->
            let
                wip =
                    state.wip

                newWip =
                    { wip | description = str }
            in
                { state | needsSaving = True, wip = newWip } ! []

        ZoomIn ->
            let
                state_ =
                    { state | zoomLevel = state.zoomLevel + 0.2 }
            in
                state_ ! [ scaleGraph state_.zoomLevel ]

        ZoomActualSize ->
            let
                state_ =
                    { state | zoomLevel = 1.0 }
            in
                state_ ! [ scaleGraph state_.zoomLevel ]

        ZoomOut ->
            let
                state_ =
                    { state | zoomLevel = state.zoomLevel - 0.2 }
            in
                state_ ! [ scaleGraph state_.zoomLevel ]

        ModelSearch searchMsg ->
            updateModelSearch searchMsg state

        ExecutionInputs inputMsg ->
            updateExecutionInputs inputMsg state

        UIMsg uiMsg ->
            updateFromUI uiMsg state


initializePage : Int -> Navigation.Location -> ( State, Cmd Msg.Msg )
initializePage seed loc =
    let
        uuid =
            UrlParser.parseHash UrlParser.string loc |> Maybe.withDefault ""
    in
        State.init seed |> Return.andThen (doLoadAllModels uuid)


main : Program Int State Msg.Msg
main =
    Navigation.programWithFlags LoadPage
        { init = initializePage
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
