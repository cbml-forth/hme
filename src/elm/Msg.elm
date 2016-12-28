module Msg exposing (Msg(..))

import Rest
import State
import Ports


type alias ModelSearchPerspectiveCrit =
    { uri : String, value : Maybe String }


type Msg
    = ZoomIn
    | ZoomOut
    | ZoomActualSize
    | Export
    | LoadHypermodels
    | NewHypermodel
    | SaveHypermodel
    | ReloadHypermodel
    | LoadModels
    | Refresh
    | RefreshResponse (Rest.Msg Rest.HyperModels)
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
    | ModelSearchPerspective ModelSearchPerspectiveCrit
