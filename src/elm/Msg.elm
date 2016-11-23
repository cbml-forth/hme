module Msg exposing (Msg(..))

import Rest
import State
import Ports


{--
type ServerResponseMsg
    = HyperModelsResponse HyperModels
    | ModelsResponse Models
    | HypermodelSaveResponse String
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
    | RestResponse Rest.Return
    | CloseModal String
    | OpenHypermodel String
    | AddModel State.Model
    | ChangeTitle String
    | ChangeDescription String
    | ModelSearchTitle String
    | ModelSearchFrozen Bool
    | ModelSearchStronglyCoupled Bool
    | UIMsg Ports.Msg
