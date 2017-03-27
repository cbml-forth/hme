module ValidateHypermodel exposing (..)

import State
import Graph
import Utils


type ValidHypermodelStatus
    = ValidHypermodel
    | InvalidHypermodel (List String)


validateHypermodel : List State.Model -> State.Hypermodel -> ValidHypermodelStatus
validateHypermodel allModels hm =
    let
        nodesModelPairs : List ( Graph.NodeId, String )
        nodesModelPairs =
            Graph.modelNodes hm.graph

        usedModelsPairs : List ( Graph.NodeId, State.Model )
        usedModelsPairs =
            List.filterMap (Tuple.mapSecond (State.findModelByUUID allModels) >> Utils.liftMaybeToTuple)
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
                |> List.filter (State.findModelByUUID allModels >> Utils.isNothing)

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


isValidHypermodel : List State.Model -> State.Hypermodel -> Bool
isValidHypermodel allModels hm =
    case validateHypermodel allModels hm of
        ValidHypermodel ->
            True

        _ ->
            False
