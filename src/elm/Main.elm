port module Main exposing (..)

import Html exposing (programWithFlags)
import Html exposing (Html)
import State exposing (State, updateHypermodels, findHypermodelByUUID, findΜodelByUUID)
import Rest exposing (..)
import Ports
    exposing
        ( showOrHideModal
        , loadHypermodel
        , addModelToGraph
        , serializeGraph
        , scaleGraph
        )
import Graph
import Msg exposing (..)
import View exposing (view, modalWinIds)
import Return exposing ((>>>))
import RemoteData


subscriptions : State -> Sub Msg.Msg
subscriptions model =
    Ports.subscriptions model |> Sub.map UIMsg


debugView : State -> Html Msg.Msg
debugView state =
    Debug.log "New State:" state |> View.view


loadHypermodel : State.Hypermodel -> List State.Model -> Cmd msg
loadHypermodel hm allModels =
    -- Ports.loadHypermodel { readOnly = False, jsonStr = hm.canvas }
    let
        nodes =
            Graph.nodes hm.graph

        addNode : Graph.Node -> Maybe (Cmd msg)
        addNode { id, kind, position } =
            case kind of
                Graph.ModelNode uuid ->
                    findΜodelByUUID uuid allModels |> Maybe.map (Ports.addModelToGraph id position)

        cmds =
            List.map
                (\x ->
                    case x of
                        Just cmd ->
                            cmd

                        Nothing ->
                            Cmd.none
                )
                (List.map addNode nodes)
    in
        Cmd.batch (Debug.log "CMDS=" cmds)



-- loadHypermodel : String -> Cmd msg
-- loadHypermodel js =
--     Ports.loadHypermodel { readOnly = False, jsonStr = js }


doLoadHypermodels : State -> ( State, Cmd Msg.Msg )
doLoadHypermodels state =
    { state
        | pendingRestCalls = state.pendingRestCalls + 1
        , busyMessage = "Retrieving hypermodels.."
    }
        ! [ Cmd.map RestResponse Rest.getHyperModels
          ]


doLoadModels : State -> ( State, Cmd Msg.Msg )
doLoadModels state =
    { state
        | pendingRestCalls = state.pendingRestCalls + 1
        , busyMessage = "Retrieving models.."
    }
        ! [ Cmd.map RestResponse Rest.getModels
          ]


updateFromServer : State -> Rest.ServerResponseMsg -> ( State, Cmd Msg.Msg )
updateFromServer state response =
    case Debug.log "REST-RESP: " response of
        Rest.HyperModelsResponse list ->
            let
                updateHypermodelsState =
                    State.updateHypermodels list state
            in
                { updateHypermodelsState | showHypermodels = True }
                    ! [ showOrHideModal True modalWinIds.listHypermodels ]

        Rest.ModelsResponse models ->
            let
                ms =
                    List.sortBy .title models
            in
                { state | allModels = RemoteData.Success ms } ! []

        Rest.HypermodelSaveResponse version ->
            { state | needsSaving = False } ! []


update : Msg.Msg -> State -> ( State, Cmd Msg.Msg )
update m state =
    case Debug.log "MSG:" m of
        NewHypermodel ->
            let
                newState =
                    State.newHypermodel state

                allModels =
                    RemoteData.withDefault [] newState.allModels
            in
                newState ! [ loadHypermodel newState.wip allModels ]

        LoadHypermodels ->
            doLoadHypermodels state

        LoadModels ->
            -- { state
            --     | pendingRestCalls = state.pendingRestCalls + 1
            --     , busyMessage = "Loading models.."
            -- }
            state
                ! [ -- Rest.getModels |> Cmd.map RestResponse
                    showOrHideModal True modalWinIds.listModels
                  ]

        RestResponse response ->
            let
                newState =
                    { state
                        | pendingRestCalls = state.pendingRestCalls - 1
                        , busyMessage = ""
                    }
            in
                case response of
                    Err error ->
                        { newState | serverError = Just error } ! [ showOrHideModal True modalWinIds.errorAlert ]

                    Ok return ->
                        updateFromServer newState return

        CloseModal modalId ->
            { state | showModels = False, showHypermodels = False } ! [ showOrHideModal False modalId ]

        OpenHypermodel uuid ->
            let
                hm =
                    State.findHypermodelByUUID uuid state.allHypermodels

                allModels =
                    RemoteData.withDefault [] state.allModels

                wip_ =
                    case hm of
                        Just hypermodel ->
                            hypermodel

                        Nothing ->
                            state.wip
            in
                { state
                    | needsSaving = False
                    , showHypermodels = False
                    , loadedHypermodel = hm
                    , wip = wip_
                }
                    ! [ showOrHideModal False modalWinIds.listHypermodels
                        --   , hm |> Maybe.map .canvas |> Maybe.withDefault "" |> loadHypermodel
                      , loadHypermodel wip_ allModels
                      ]

        ReloadHypermodel ->
            let
                uuid =
                    state.loadedHypermodel |> Maybe.map .id |> Maybe.withDefault ""

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
                    | -- graph = Graph.newGraph uuid,
                      needsSaving = False
                    , wip = newWip
                    , loadedHypermodel = hm
                }
                    -- ! [ hm |> Maybe.map .canvas |> Maybe.withDefault "" |> loadHypermodel ]
                    !
                        [ loadHypermodel newWip allModels ]

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
                    Debug.log "WIP = " { wip | graph = newGraph2 }
            in
                { state | wip = newWip, needsSaving = True }
                    ! [ showOrHideModal False modalWinIds.listModels
                      , Ports.addModelToGraph nodeId pos model
                      ]

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
                newState
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

        ModelSearchTitle str ->
            let
                search =
                    state.modelSearch

                search_ =
                    { search | title = Just str }
            in
                { state | modelSearch = search_ } ! [ showOrHideModal True modalWinIds.listModels ]

        ModelSearchFrozen b ->
            let
                search =
                    state.modelSearch

                search_ =
                    { search | frozenOnly = b }
            in
                { state | modelSearch = search_ } ! [ showOrHideModal True modalWinIds.listModels ]

        ModelSearchStronglyCoupled b ->
            let
                search =
                    state.modelSearch

                search_ =
                    { search | stronglyCoupledOnly = b }
            in
                { state | modelSearch = search_ } ! [ showOrHideModal True modalWinIds.listModels ]

        UIMsg uiMsg ->
            case uiMsg of
                Ports.NewGraph { canvas, svg } ->
                    let
                        wip =
                            state.wip

                        newWip =
                            { wip | canvas = canvas, svgContent = svg }
                    in
                        { state | needsSaving = True, wip = newWip }
                            ! [ Cmd.map RestResponse (Rest.saveHyperModel newWip)
                              ]

                Ports.NewConnection conn ->
                    let
                        wip =
                            state.wip

                        newGraph =
                            Graph.addConnection conn wip.graph
                                |> (\g ->
                                        let
                                            js =
                                                Debug.log "" (Graph.graphToJson g)
                                        in
                                            g
                                   )

                        newWip =
                            { wip | graph = newGraph }
                    in
                        { state | needsSaving = True, wip = newWip } ! []

                Ports.MoveNode nodeId position ->
                    let
                        wip =
                            state.wip

                        newGraph =
                            Graph.moveNode nodeId position wip.graph

                        -- |> Debug.log "GRAPH = "
                        newWip =
                            { wip | graph = newGraph }
                    in
                        { state | needsSaving = True, wip = newWip } ! []

                Ports.RemoveNode nodeId ->
                    let
                        wip =
                            state.wip

                        newGraph =
                            Graph.removeNode nodeId wip.graph

                        newWip =
                            { wip | graph = newGraph }

                        -- |> Debug.log "GRAPH = "
                    in
                        { state | needsSaving = True, wip = newWip } ! []

                Ports.ShowNode nodeId ->
                    state ! []

                Ports.RemoveConnection connId ->
                    let
                        wip =
                            state.wip

                        newGraph =
                            Graph.removeConnection connId wip.graph

                        -- |> Debug.log "GRAPH = "
                        newWip =
                            { wip | graph = newGraph }
                    in
                        { state | needsSaving = True, wip = newWip } ! []

        _ ->
            state ! []


main : Program Int State Msg.Msg
main =
    programWithFlags
        { init =
            State.init >>> doLoadModels
        , update = update
        , subscriptions =
            subscriptions
            -- , view = debugView
        , view = view
        }
