port module Main exposing (..)

import Html exposing (programWithFlags)
import Html exposing (Html)
import State
    exposing
        ( State
        , updateHypermodels
        , findHypermodelByUUID
        , findΜodelByUUID
        )
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

        modelToJson : String -> Graph.Position -> State.Model -> Encode.Value
        modelToJson nodeId pos model =
            let
                inPorts =
                    List.map (Encode.string << .name) model.inPorts |> Encode.list

                outPorts =
                    List.map (Encode.string << .name) model.outPorts |> Encode.list

                dynPorts =
                    model.inPorts ++ model.outPorts |> List.filter .is_dynamic |> List.map (.name >> Encode.string) |> Encode.list

                position =
                    Encode.object [ ( "x", Encode.int pos.x ), ( "y", Encode.int pos.y ) ]
            in
                Encode.object
                    [ ( "id", Encode.string nodeId )
                    , ( "name", Encode.string model.title )
                    , ( "ports"
                      , Encode.object
                            [ ( "inPorts", inPorts )
                            , ( "outPorts", outPorts )
                            , ( "dynPorts", dynPorts )
                            ]
                      )
                    , ( "position", position )
                    ]

        nodeToJson : Graph.Node -> Maybe Encode.Value
        nodeToJson { id, kind, position } =
            case kind of
                Graph.ModelNode uuid ->
                    findΜodelByUUID uuid allModels |> Maybe.map (modelToJson id position)

        nodesListJson =
            List.filterMap nodeToJson (Graph.nodes hm.graph)

        connsListJson =
            List.map Graph.encodeConnection (Graph.connections hm.graph)
    in
        Ports.loadHypermodel2 (Encode.object [ ( "nodes", Encode.list nodesListJson ), ( "links", Encode.list connsListJson ) ])


doLoadHypermodels : State -> ( State, Cmd Msg.Msg )
doLoadHypermodels state =
    { state
        | pendingRestCalls = state.pendingRestCalls + 1
        , busyMessage = "Retrieving hypermodels.."
    }
        ! [ Cmd.map HyperModelsResponse Rest.getHyperModels
          ]


doLoadModels : State -> ( State, Cmd Msg.Msg )
doLoadModels state =
    { state
        | pendingRestCalls = state.pendingRestCalls + 1
        , busyMessage = "Retrieving models.."
    }
        ! [ Cmd.map ModelsResponse Rest.getModels
          ]


serverUpdate : Rest.Msg a -> State.State -> ( State.State, Cmd Msg.Msg )
serverUpdate response state =
    let
        calls =
            state.pendingRestCalls - 1

        newState =
            { state
                | pendingRestCalls = calls
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


update : Msg.Msg -> State -> ( State, Cmd Msg.Msg )
update m state =
    case Debug.log "MSG:" m of
        NewHypermodel ->
            startNewHypermodel state

        LoadHypermodels ->
            doLoadHypermodels state

        LoadModels ->
            showLoadedModels state

        HyperModelsResponse response ->
            let
                ( newState, cmds ) =
                    serverUpdate response state
            in
                case response of
                    Ok (Rest.HyperModels list) ->
                        let
                            newState2 =
                                State.updateHypermodels list newState
                        in
                            { newState2 | showHypermodels = True }
                                ! [ cmds, showOrHideModal True modalWinIds.listHypermodels ]

                    _ ->
                        ( newState, cmds )

        ModelsResponse response ->
            let
                ( newState, cmds ) =
                    serverUpdate response state
            in
                case response of
                    Ok (Rest.Models models) ->
                        let
                            ms =
                                List.sortBy .title models

                            newCmds =
                                if RemoteData.isLoading newState.allModels then
                                    [ showOrHideModal True modalWinIds.listModels, cmds ]
                                else
                                    [ cmds ]
                        in
                            { newState | allModels = RemoteData.Success ms } ! newCmds

                    _ ->
                        ( newState, cmds )

        HypermodelSaveResponse response ->
            let
                ( newState, cmds ) =
                    serverUpdate response state
            in
                case response of
                    Ok (Rest.Version _) ->
                        { newState | needsSaving = False } ! [ cmds ]

                    _ ->
                        newState ! [ cmds ]

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
                    | needsSaving = False
                    , wip = newWip
                    , loadedHypermodel = hm
                }
                    ! [ loadHypermodel newWip allModels ]

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
                            ! [ Cmd.map HypermodelSaveResponse (Rest.saveHyperModel newWip)
                              ]

                Ports.NewConnection conn ->
                    let
                        wip =
                            state.wip

                        _ =
                            Debug.log "CONN: " conn

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
