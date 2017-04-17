module Rest exposing (..)

import Decoders
import Encoders
import Http
import HttpBuilder
import Json.Decode as Decode
import Json.Encode as Encode
import State
import Task


server : String
server =
    "/hme2/api"


modelsUrl : String
modelsUrl =
    server ++ "/models"


hyperModelsUrl : String
hyperModelsUrl =
    server ++ "/hypermodels"


experimentsUrl : String
experimentsUrl =
    server ++ "/experiments"


downloadExperimentUri : String -> String
downloadExperimentUri uuid =
    "/hme2/results?uuid=" ++ uuid


type Models
    = Models (List State.Model)


type HyperModels
    = HyperModels (List State.Hypermodel)


type HypoHyperModels
    = HypoHyperModels Models HyperModels


type Version
    = Version String String


type alias Msg a =
    Result Http.Error a


type alias ExecuteHypermodelRequest =
    { hypermodelId : String
    , version : String
    , xmml : String
    , inputs : List State.ModelInputWithValue
    , outputs : List State.ModelInOutput
    , isStronglyCoupled : Bool
    }


type alias SaveHypermodelRequest =
    { hypermodel : State.Hypermodel
    , isStronglyCoupled : Bool
    }


getResource : String -> Decode.Decoder a -> Http.Request a
getResource url decoder =
    HttpBuilder.get url
        |> HttpBuilder.withHeader "X-Requested-By" "elm"
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


sendRequest : Http.Request a -> Cmd (Msg a)
sendRequest req =
    Http.send identity req


getModels_ : Http.Request Models
getModels_ =
    getResource modelsUrl (Decode.list Decoders.modelDecoder |> Decode.map Models)


getModels : Cmd (Msg Models)
getModels =
    sendRequest getModels_


getExperiments : Cmd (Msg State.Experiments)
getExperiments =
    sendRequest getExperiments_


getHyperModels_ : Http.Request HyperModels
getHyperModels_ =
    getResource hyperModelsUrl (Decode.list Decoders.hypermodelDecoder |> Decode.map HyperModels)


getExperiments_ : Http.Request State.Experiments
getExperiments_ =
    getResource experimentsUrl (Decode.list Decoders.experimentDecoder)


getHyperModels : Cmd (Msg HyperModels)
getHyperModels =
    sendRequest getHyperModels_


getAllModels : Cmd (Msg HypoHyperModels)
getAllModels =
    let
        modelsTask =
            Http.toTask getModels_

        hypermodelsTask =
            Http.toTask getHyperModels_
    in
        Task.map2 HypoHyperModels modelsTask hypermodelsTask |> Task.attempt identity


saveHyperModel : SaveHypermodelRequest -> Cmd (Msg Version)
saveHyperModel { hypermodel, isStronglyCoupled } =
    let
        etag =
            "\"" ++ hypermodel.version ++ "\""

        maybeAddMatchHeader =
            if String.isEmpty hypermodel.version then
                identity
            else
                HttpBuilder.withHeader "If-Match" etag

        decoder =
            saveResponseDecoder hypermodel.id
    in
        HttpBuilder.post hyperModelsUrl
            |> maybeAddMatchHeader
            |> HttpBuilder.withHeader "X-Requested-By" "elm"
            |> HttpBuilder.withJsonBody (Encoders.encodeHypermodel isStronglyCoupled hypermodel)
            |> HttpBuilder.withExpect (Http.expectJson decoder)
            |> HttpBuilder.send identity


saveResponseDecoder : String -> Decode.Decoder Version
saveResponseDecoder hypermodelUuid =
    Decode.at [ "version" ] Decode.string |> Decode.map (Version hypermodelUuid)


publishHypermodel : ExecuteHypermodelRequest -> Cmd (Msg State.Experiment)
publishHypermodel { hypermodelId, version, xmml, inputs, outputs, isStronglyCoupled } =
    let
        body : Encode.Value
        body =
            Encode.object
                [ ( "uuid", Encode.string hypermodelId )
                , ( "version", Encode.string version )
                , ( "xmml", Encode.string xmml )
                , ( "isStronglyCoupled", Encode.bool isStronglyCoupled )
                , ( "inputs", List.map Encoders.encodeModelInputWithValue inputs |> Encode.list )
                , ( "outputs", List.map (Encoders.encodeModelParameter True) outputs |> Encode.list )
                ]

        uri =
            server ++ "/publishedhypermodels/" ++ hypermodelId
    in
        HttpBuilder.put uri
            |> HttpBuilder.withHeader "X-Requested-By" "elm"
            |> HttpBuilder.withJsonBody body
            |> HttpBuilder.withExpect (Http.expectJson Decoders.experimentDecoder)
            |> HttpBuilder.send identity
