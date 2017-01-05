module Msg exposing (Msg(..))

import Rest
import State
import Ports
import Navigation


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
    | StateInitResponse String (Rest.Msg Rest.HypoHyperModels)
    | HypermodelSaveResponse (Rest.Msg Rest.Version)
    | OpenHypermodelResponse String (Rest.Msg Rest.HyperModels)
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
    | LoadPage Navigation.Location
