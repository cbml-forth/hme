module Main exposing (..)

import AllDict
import Dict
import Set
import Graph exposing (nodeIdDecoder)
import Html exposing (Html)
import Json.Encode as Encode
import List.Extra
import Msg exposing (..)
import Navigation
import Ports exposing (addModelToGraph, loadHypermodel, scaleGraph, serializeGraph, showOrHideModal, showNotification)
import RemoteData
import Rest exposing (..)
import Return exposing ((>>>))
import State exposing (..)
import UrlParser
import Utils exposing ((=>))
import ValidateHypermodel exposing (ValidHypermodelStatus(..))
import View exposing (modalWinIds, view)
import Uuid
import Xmml


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
                paramToJson { name, dataType, isDynamic } =
                    Encode.object
                        [ ( "name", Encode.string name )
                        , ( "dataType", Encode.string dataType )
                        , ( "isDynamic", Encode.bool isDynamic )
                        ]

                inPorts =
                    List.map paramToJson model.inPorts |> Encode.list

                outPorts =
                    List.map paramToJson model.outPorts |> Encode.list

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
                , "label" => Encode.string "2 hr"
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


doLoadExperiments : (Rest.Msg State.Experiments -> Msg.Msg) -> State -> ( State, Cmd Msg.Msg )
doLoadExperiments tagger state =
    { state
        | pendingRestCalls = state.pendingRestCalls + 1
        , busyMessage = "Retrieving experiments.."
    }
        ! [ Cmd.map tagger Rest.getExperiments
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
                    |> Maybe.map (ValidateHypermodel.validateHypermodel allModels)
                    |> Maybe.withDefault ValidHypermodel
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

                stronglyCoupled =
                    state.allModels
                        |> RemoteData.map (State.hypermodelIsStronglyCoupled wip)
                        |> RemoteData.withDefault False

                request : Rest.SaveHypermodelRequest
                request =
                    { hypermodel = newWip
                    , isStronglyCoupled = stronglyCoupled
                    }
            in
                newState
                    ! [ Cmd.map HypermodelSaveResponse (Rest.saveHyperModel request)
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

        Ports.Notification { uuid, title, status } ->
            let
                el =
                    "exp-" ++ uuid

                s =
                    "'" ++ title ++ "': " ++ toString status

                msg =
                    Ports.animateElement el

                msg2 =
                    Ports.showNotification { message = s, experimentId = uuid }
            in
                { state
                    | hotExperiments = Dict.insert uuid status state.hotExperiments |> Debug.log "Notifications "
                    , notificationCount = state.notificationCount + 1
                }
                    ! [ msg, msg2 ]

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

        ModelSearchComposite b ->
            { search | showCompositeOnly = b }

        ModelSearchPerspective { uri, value } ->
            State.updateModelSearchPersp search uri value

        ClearSearch ->
            State.initModelSearch


updateExecutionInputs : Msg.ExecutionInputsMsg -> State.State -> State.ExecutionInfo
updateExecutionInputs executionInputsMsgMsg ({ executionInfo } as state) =
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
            UseCaching nodeId enabled ->
                let
                    caching =
                        State.toggleCaching executionInfo.useCaching nodeId enabled
                in
                    { executionInfo | useCaching = caching }

            ClearAllInputs ->
                { executionInfo | inputs = State.emptyExecutionInputs }

            ClearInputsOf nodeId ->
                { executionInfo | inputs = AllDict.remove nodeId executionInfo.inputs }

            DoFillDefaultInputs ->
                let
                    newExc : State.HypermodelExecutionInput
                    newExc =
                        List.foldl
                            (\( nodeId, model ) newD ->
                                AllDict.update nodeId (Just << updateWithDefaultInputs model) newD
                            )
                            executionInfo.inputs
                            usedModels
                in
                    { executionInfo | inputs = newExc }

            DoFillDefaultInputsOf nodeId ->
                let
                    maybeModel : Maybe State.Model
                    maybeModel =
                        Utils.listFind ((==) nodeId << Tuple.first) usedModels
                            |> Maybe.map Tuple.second

                    newExc =
                        case maybeModel of
                            Nothing ->
                                executionInfo.inputs

                            Just model ->
                                AllDict.update nodeId (Just << updateWithDefaultInputs model) executionInfo.inputs
                in
                    { executionInfo | inputs = newExc }

            FilledInput nodeId param value ->
                let
                    newExc =
                        AllDict.update nodeId
                            (Maybe.map (State.overrideFilledInputs param value)
                                >> Maybe.withDefault (Dict.singleton param value)
                                >> Just
                            )
                            executionInfo.inputs
                in
                    { executionInfo | inputs = newExc }


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
        executionInputs =
            state.executionInfo.inputs

        wip =
            state.wip

        cached =
            Set.toList state.executionInfo.useCaching

        xmml =
            RemoteData.map (Xmml.toXmmlString wip.title wip.graph cached) state.allModels |> RemoteData.withDefault ""

        freeInputs : List ( Graph.Node, List ( State.ModelInOutput, String ) )
        freeInputs =
            RemoteData.map (State.inputsOfHypermodelNewNames wip.graph) state.allModels |> RemoteData.withDefault []

        instanceId : Graph.NodeId -> String
        instanceId (Graph.NodeId id) =
            "i" ++ toString id

        inputsWithValues : Graph.Node -> ( State.ModelInOutput, String ) -> State.ModelInputWithValue
        inputsWithValues { id } ( modelParam, newName ) =
            let
                defaultValue =
                    Maybe.withDefault "" modelParam.defaultValue

                modelParam2 =
                    { modelParam | name = newName }

                value =
                    State.executionInputFor executionInputs id modelParam.name |> Maybe.withDefault defaultValue
            in
                State.ModelInputWithValue modelParam2 value

        freeOutputs : List ( Graph.Node, List State.ModelInOutput )
        freeOutputs =
            RemoteData.map (State.outputsOfHypermodelNewNames wip.graph) state.allModels
                |> RemoteData.withDefault []
                |> List.map (Tuple.mapSecond (List.map (\( p, newname ) -> { p | name = newname })))

        -- List.map Tuple.second freeInputs |> List.concat
        inputs =
            List.concatMap (\( node, listofParams ) -> List.map (inputsWithValues node) listofParams) freeInputs

        outputs =
            List.concatMap Tuple.second freeOutputs

        ( inputsUuids, state1 ) =
            State.newUuids (List.length inputs) state

        ( outputsUuids, state2 ) =
            State.newUuids (List.length outputs) state1

        newState =
            { state2
                | pendingRestCalls = state.pendingRestCalls + 1
                , busyMessage = "publishing hypermodel.."
            }

        stronglyCoupled =
            state.allModels
                |> RemoteData.map (State.hypermodelIsStronglyCoupled wip)
                |> RemoteData.withDefault False

        inputsWithNewUuids =
            List.Extra.zip inputs inputsUuids
                |> List.map
                    (\( State.ModelInputWithValue p value, u ) ->
                        let
                            newp =
                                { p | repoId = 0, uuid = Uuid.toString u }
                        in
                            State.ModelInputWithValue newp value
                    )

        outputsWithNewUuids =
            List.Extra.zip outputs outputsUuids
                |> List.map (\( p, u ) -> { p | repoId = 0, uuid = Uuid.toString u })

        request : Rest.ExecuteHypermodelRequest
        request =
            { hypermodelId = wip.id
            , version = wip.version
            , xmml = xmml
            , inputs = inputsWithNewUuids
            , outputs = outputsWithNewUuids
            , isStronglyCoupled = stronglyCoupled
            }
    in
        { state
            | pendingRestCalls = state.pendingRestCalls + 1
            , busyMessage = "Submitting hypermodel to execution framework.."
        }
            ! [ Cmd.map PublishHypermodelResponse (Rest.publishHypermodel request) ]


publishHypermodelOld : State.State -> ( State.State, Cmd Msg.Msg )
publishHypermodelOld state =
    let
        executionInputs =
            state.executionInfo.inputs

        wip =
            state.wip

        cached =
            Set.toList state.executionInfo.useCaching

        xmml =
            RemoteData.map (Xmml.toXmmlString wip.title wip.graph cached) state.allModels |> RemoteData.withDefault ""

        freeInputs : List ( Graph.Node, List State.ModelInOutput )
        freeInputs =
            RemoteData.map (State.freeInputsOfHypermodel wip.graph) state.allModels |> RemoteData.withDefault []

        instanceId : Graph.NodeId -> String
        instanceId (Graph.NodeId id) =
            "i" ++ toString id

        inputsWithValues : Graph.Node -> State.ModelInOutput -> State.ModelInputWithValue
        inputsWithValues { id } modelParam =
            let
                instance =
                    instanceId id

                defaultValue =
                    Maybe.withDefault "" modelParam.defaultValue

                modelParam2 =
                    { modelParam | name = instance ++ "_" ++ modelParam.name }

                value =
                    State.executionInputFor executionInputs id modelParam.name |> Maybe.withDefault defaultValue
            in
                State.ModelInputWithValue modelParam2 value

        freeOutputs : List ( Graph.Node, List State.ModelInOutput )
        freeOutputs =
            RemoteData.map (State.freeOutputsOfHypermodel wip.graph) state.allModels |> RemoteData.withDefault []

        -- List.map Tuple.second freeInputs |> List.concat
        inputs =
            List.concatMap (\( node, listofParams ) -> List.map (inputsWithValues node) listofParams) freeInputs

        outputs =
            let
                createNodeIdSpecificNames : ( Graph.Node, List State.ModelInOutput ) -> List State.ModelInOutput
                createNodeIdSpecificNames ( { id }, params ) =
                    let
                        instance =
                            instanceId id
                    in
                        params |> List.map (\modelParam -> { modelParam | name = instance ++ "_" ++ modelParam.name })
            in
                List.map createNodeIdSpecificNames freeOutputs |> List.concat

        ( inputsUuids, state1 ) =
            State.newUuids (List.length inputs) state

        ( outputsUuids, state2 ) =
            State.newUuids (List.length outputs) state1

        newState =
            { state2
                | pendingRestCalls = state.pendingRestCalls + 1
                , busyMessage = "publishing hypermodel.."
            }

        stronglyCoupled =
            state.allModels
                |> RemoteData.map (State.hypermodelIsStronglyCoupled wip)
                |> RemoteData.withDefault False

        inputsWithNewUuids =
            List.Extra.zip inputs inputsUuids
                |> List.map
                    (\( State.ModelInputWithValue p value, u ) ->
                        let
                            newp =
                                { p | repoId = 0, uuid = Uuid.toString u }
                        in
                            State.ModelInputWithValue newp value
                    )

        outputsWithNewUuids =
            List.Extra.zip outputs outputsUuids
                |> List.map (\( p, u ) -> { p | repoId = 0, uuid = Uuid.toString u })

        request : Rest.ExecuteHypermodelRequest
        request =
            { hypermodelId = wip.id
            , version = wip.version
            , xmml = xmml
            , inputs = inputsWithNewUuids
            , outputs = outputsWithNewUuids
            , isStronglyCoupled = stronglyCoupled
            }
    in
        { state
            | pendingRestCalls = state.pendingRestCalls + 1
            , busyMessage = "Submitting hypermodel to execution framework.."
        }
            ! [ Cmd.map PublishHypermodelResponse (Rest.publishHypermodel request) ]


update : Msg.Msg -> State -> ( State, Cmd Msg.Msg )
update m state =
    case m of
        ShowExperiments ->
            doLoadExperiments ExperimentsResponse state

        ExperimentsResponse response ->
            state
                |> filterResponseUpdate response
                    (\exps state ->
                        { state | experiments = exps }
                            |> showModal State.ShowExperimentsWin
                    )

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
            state
                |> filterResponseUpdate response
                    (\({ experimentRepoId, title, status } as experiment) state ->
                        let
                            titleWin =
                                "Execution info"

                            message =
                                "Hypermodel '" ++ title ++ "' has been submitted for execution"

                            newState =
                                { state | infoMessage = ( titleWin, message ) } |> State.newExperiment experiment
                        in
                            showModal InfoWin newState
                    )

        CloseModal modalId ->
            let
                newState =
                    if modalId == ShowExperimentsWin then
                        { state | hotExperiments = Dict.empty, notificationCount = 0 }
                    else
                        state
            in
                hideModal modalId newState

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

                cached =
                    Set.toList state.executionInfo.useCaching

                mml =
                    RemoteData.map (Xmml.toXmmlString wip.title wip.graph cached) state.allModels |> RemoteData.withDefault ""
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

        ShowFillInputsDialog ->
            showModal State.LaunchExecutionWin state

        ExecutionInputs inputMsg ->
            let
                newExecState =
                    updateExecutionInputs inputMsg state
            in
                { state | executionInfo = newExecState } ! []

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
