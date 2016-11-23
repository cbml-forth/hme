module Msg exposing (Msg(..))

import Rest
import State
import Ports


{--
type ServerResponseMsg
    = HyperModelsResponse HyperModels
    | ModelsResponse Models
    | HypermodelSaveResponse String



type ServerResponseMsg
    = HyperModelsResponse (Rest.Msg Rest.HyperModels)
    | ModelsResponse (Rest.Msg Rest.Models)
    | HypermodelSaveResponse (Rest.Msg String)

--}


type Msg
    = Empty
    | ZoomIn
    | ZoomOut
    | ZoomActualSize
    | LoadHypermodels
    | NewHypermodel
    | SaveHypermodel
    | ReloadHypermodel
    | LoadModels
      {--
    | ModelsServerResponse Rest.ModelsResponse
    | HyperModelsServerResponse Rest.HyperModelsResponse
    | HypermodelSaved Rest.HyperModelSaveResponse --}
    | DoSaveHypermodel
    | HyperModelsResponse (Rest.Msg Rest.HyperModels)
    | ModelsResponse (Rest.Msg Rest.Models)
    | HypermodelSaveResponse (Rest.Msg Rest.Version)
    | CloseModal String
    | OpenHypermodel String
    | AddModel State.Model
    | ChangeTitle String
    | ChangeDescription String
    | ModelSearchTitle String
    | ModelSearchFrozen Bool
    | ModelSearchStronglyCoupled Bool
    | UIMsg Ports.Msg
