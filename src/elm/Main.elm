module Main exposing (..)

import AllDict
import Dict
import Graph exposing (nodeIdDecoder)
import Html exposing (Html)
import Json.Encode as Encode
import List.Extra
import Msg exposing (..)
import Navigation
import Ports exposing (addModelToGraph, loadHypermodel, scaleGraph, serializeGraph, showOrHideModal)
import RemoteData
import Rest exposing (..)
import Return exposing ((>>>))
import State exposing (..)
import UrlParser
import Utils exposing ((=>))
import View exposing (modalWinIds, view)
import Xmml


subscriptions : State -> Sub Msg.Msg
subscriptions model =
    Ports.subscriptions model |> Sub.map UIMsg


debugView : State -> Html Msg.Msg
debugView state =
    Debug.log "New State:" state |> View.view


type ValidHypermodelStatus
    = ValidHypermodel
    | InvalidHypermodel (List String)


isValidHypermodel : List State.Model -> State.Hypermodel -> ValidHypermodelStatus
isValidHypermodel allModels hm =
    let
        nodesModelPairs : List ( Graph.NodeId, String )
        nodesModelPairs =
            Graph.modelNodes hm.graph

        usedModelsPairs : List ( Graph.NodeId, State.Model )
        usedModelsPairs =
            List.filterMap (Tuple.mapSecond (findModelByUUID allModels) >> Utils.liftMaybeToTuple)
                nodesModelPairs

        missingConnections : List ( State.Model, List String )
        missingConnections =
            List.filterMap
                (\( nodeId, model ) ->
                    let
                        connectedInputs =
                            Graph.connectedInputsOfNode nodeId hm.graph

                        connectedOutputs =
                            Graph.connectedOutputsOfNode nodeId hm.graph

                        missingInputs =
                            List.filter (\param -> State.findInputParam model param == Nothing) connectedInputs

                        missingOutputs =
                            List.filter (\param -> State.findOutputParam model param == Nothing) connectedOutputs

                        missingParams =
                            missingInputs ++ missingOutputs
                    in
                        if List.isEmpty missingParams then
                            Nothing
                        else
                            Just ( model, missingParams )
                )
                usedModelsPairs

        missingModels =
            List.map Tuple.second nodesModelPairs
                |> List.filter (findModelByUUID allModels >> Utils.isNothing)

        missingModelsErrors =
            String.join "," missingModels |> (++) "Missing models: "

        missingConnectionsErrors =
            missingConnections
                |> List.map (Tuple.mapSecond (String.join ","))
                |> List.map
                    (\( { title, uuid }, paramsStr ) ->
                        "Model "
                            ++ uuid
                            ++ " ("
                            ++ title
                            ++ ") lacks the following parameters: "
                            ++ paramsStr
                    )

        errors =
            if List.isEmpty missingModels then
                missingConnectionsErrors
            else
                missingModelsErrors :: missingConnectionsErrors
    in
        if List.isEmpty errors then
            ValidHypermodel
        else
            InvalidHypermodel errors


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
                    findModelByUUID allModels uuid |> Maybe.map (modelToJson id position)

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
        Ports.loadHypermodel
            (Encode.object
                [ "nodes" => Encode.list nodesListJson
                , "links" => Encode.list connsListJson
                , "title" => Encode.string hm.title
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


showModal : ModalWin -> State -> ( State, Cmd Msg.Msg )
showModal modalWin ({ modalsState } as state) =
    let
        isOpen =
            Utils.listContains modalWin modalsState.openModals

        newModalsState =
            if isOpen then
                modalsState
            else
                { modalsState | openModals = modalWin :: modalsState.openModals }

        cmd =
            showOrHideModal True (modalWinIds modalWin)
    in
        { state | modalsState = newModalsState } ! [ cmd ]


hideModal : ModalWin -> State -> ( State, Cmd Msg.Msg )
hideModal modalWin ({ modalsState } as state) =
    let
        isOpen =
            Utils.listContains modalWin modalsState.openModals

        newModalsState =
            if isOpen then
                { modalsState | openModals = List.Extra.remove modalWin modalsState.openModals }
            else
                modalsState

        cmd =
            if isOpen then
                showOrHideModal False (modalWinIds modalWin)
            else
                Cmd.none
    in
        { state | modalsState = newModalsState } ! [ cmd ]


serverUpdate : Rest.Msg a -> State.State -> ( State.State, Cmd Msg.Msg )
serverUpdate response state =
    let
        calls =
            state.pendingRestCalls - 1

        newState =
            { state
                | pendingRestCalls = calls
                , serverError = NoError
                , busyMessage =
                    if calls == 0 then
                        ""
                    else
                        state.busyMessage
            }
    in
        case response of
            Err httpError ->
                { newState | serverError = State.HttpError httpError } |> showModal ErrorWin

            -- ! [ showOrHideModal True modalWinIds.errorAlert ]
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
            state |> showModal State.ListModelsWin

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
            RemoteData.withDefault [] state.allHypermodels |> State.findHypermodelByUUID uuid

        allModels =
            RemoteData.withDefault [] state.allModels

        newWip =
            case hm of
                Just hypermodel ->
                    hypermodel

                Nothing ->
                    state.wip

        errors : ValidHypermodelStatus
        errors =
            if List.isEmpty allModels then
                ValidHypermodel
            else
                hm
                    |> Maybe.map (isValidHypermodel allModels)
                    |> Maybe.withDefault ValidHypermodel
                    |> Debug.log "VALID?"
    in
        case errors of
            ValidHypermodel ->
                { state
                    | needsSaving = False
                    , wip = newWip
                    , loadedHypermodel = hm
                }
                    ! [ loadHypermodel newWip allModels
                      ]

            InvalidHypermodel listofErrors ->
                { state | serverError = State.OtherError listofErrors } |> showModal ErrorWin


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
                { state | selectedNode = graphNodeId } |> showModal State.NodeDetailsWin

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


updateModelSearch : Msg.ModelSearchMsg -> State.ModelSearchState -> State.ModelSearchState
updateModelSearch modelSearchMsg search =
    case modelSearchMsg of
        ModelSearchTitle str ->
            { search | title = Just str }

        -- { state | modelSearch = search_ } ! [ showOrHideModal True modalWinIds.listModels ]
        ModelSearchFrozen b ->
            { search | frozenOnly = b }

        ModelSearchStronglyCoupled b ->
            { search | showStronglyCoupled = b }

        ModelSearchNonStronglyCoupled b ->
            { search | showNonStronglyCoupled = b }

        ModelSearchPerspective { uri, value } ->
            State.updateModelSearchPersp search uri value

        ClearSearch ->
            State.initModelSearch


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
            List.filter (not << .isDynamic) inPorts
                |> List.filterMap (Utils.on (,) .name .defaultValue >> Utils.liftMaybeToTuple)
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
            ClearAllInputs ->
                { state | executionInputs = State.emptyExecutionInputs } ! []

            ClearInputsOf nodeId ->
                { state | executionInputs = AllDict.remove nodeId state.executionInputs } ! []

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
                showModal State.LaunchExecutionWin state

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


updateZoom : Msg.ZoomMsg -> State.State -> State.State
updateZoom zoomMsg state =
    case zoomMsg of
        ZoomIn ->
            { state | zoomLevel = state.zoomLevel + 0.2 }

        ZoomActualSize ->
            { state | zoomLevel = 1.0 }

        ZoomOut ->
            { state | zoomLevel = state.zoomLevel - 0.2 }


hideAllModals : State -> ( State, Cmd Msg.Msg )
hideAllModals state =
    let
        cmds : List (Cmd a)
        cmds =
            state.modalsState.openModals |> List.map modalWinIds |> List.map (showOrHideModal False)

        newModalsState =
            { openModals = [] }
    in
        { state | modalsState = newModalsState } ! cmds


publishHypermodel : State.State -> ( State.State, Cmd Msg.Msg )
publishHypermodel state =
    let
        wip =
            state.wip

        xmml =
            RemoteData.map (Xmml.toXmmlString wip.title wip.graph) state.allModels |> RemoteData.withDefault ""

        freeInputs : List ( Graph.Node, List State.ModelInOutput )
        freeInputs =
            RemoteData.map (State.freeInputsOfHypermodel wip.graph) state.allModels |> RemoteData.withDefault []

        freeOutputs : List ( Graph.Node, List State.ModelInOutput )
        freeOutputs =
            RemoteData.map (State.freeOutputsOfHypermodel wip.graph) state.allModels |> RemoteData.withDefault []

        newState =
            { state
                | pendingRestCalls = state.pendingRestCalls + 1
                , busyMessage = "publishing hypermodel.."
            }

        request : Rest.PublishRequest
        request =
            { hypermodelId = wip.id
            , xmml = xmml
            , inputs = List.map Tuple.second freeInputs |> List.concat
            , outputs = List.map Tuple.second freeOutputs |> List.concat
            }
    in
        state ! [ Cmd.map PublishHypermodelResponse (Rest.publishHypermodel request) ]


update : Msg.Msg -> State -> ( State, Cmd Msg.Msg )
update m state =
    case m of
        PublishHypermodel ->
            publishHypermodel state

        LoadPage loc ->
            let
                uuid =
                    UrlParser.parseHash UrlParser.string loc |> Maybe.withDefault ""
            in
                if RemoteData.isNotAsked state.allHypermodels then
                    doLoadHypermodels (OpenHypermodelResponse uuid) state
                else
                    doLoadHypermodel uuid state
                        |> Return.andThen (hideModal State.ListHypermodelsWin)

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
                            |> showModal State.ListHypermodelsWin
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

        PublishHypermodelResponse response ->
            -- TODO:
            state
                |> filterResponseUpdate response (\model state -> state ! [])

        CloseModal modalId ->
            hideModal modalId state

        OpenHypermodel uuid ->
            doLoadHypermodel uuid state
                |> Return.andThen (hideModal State.ListHypermodelsWin)

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
                    ! [ Ports.addModelToGraph nodeId pos model
                      ]
                    |> Return.andThen (hideModal State.ListModelsWin)

        Export ->
            let
                wip =
                    state.wip

                mml =
                    RemoteData.map (Xmml.toXmmlString wip.title wip.graph) state.allModels |> RemoteData.withDefault ""
            in
                { state | mml = mml } |> showModal State.XMMLWin

        SaveHypermodel ->
            state |> showModal State.SaveHypermodelWin

        DoSaveHypermodel ->
            let
                newState =
                    { state
                        | pendingRestCalls = state.pendingRestCalls + 1
                        , busyMessage = "Saving hypermodel.."
                    }
            in
                state
                    ! [ Ports.serializeGraph () ]
                    |> Return.andThen (hideModal State.SaveHypermodelWin)

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

        Zoom zoomMsg ->
            updateZoom zoomMsg state ! [] |> Return.effect_ (.zoomLevel >> scaleGraph)

        ModelSearch searchMsg ->
            { state | modelSearch = updateModelSearch searchMsg state.modelSearch } ! []

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
