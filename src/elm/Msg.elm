module Msg exposing (..)

import Rest
import State
import Ports
import Navigation
import Graph


type alias ModelSearchPerspectiveCrit =
    { uri : String, value : Maybe String }


type ModelSearchMsg
    = ModelSearchTitle String
    | ModelSearchFrozen Bool
    | ModelSearchStronglyCoupled Bool
    | ModelSearchNonStronglyCoupled Bool
    | ModelSearchComposite Bool
    | ModelSearchPerspective ModelSearchPerspectiveCrit
    | ClearSearch


type ExecutionInputsMsg
    = FilledInput Graph.NodeId String String
    | DoFillDefaultInputs
    | DoFillDefaultInputsOf Graph.NodeId
    | ClearInputsOf Graph.NodeId
    | UseCaching Graph.NodeId Bool
    | ClearAllInputs


type ZoomMsg
    = ZoomIn
    | ZoomOut
    | ZoomActualSize


type Msg
    = Zoom ZoomMsg
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
    | PublishHypermodelResponse (Rest.Msg State.Experiment)
    | CloseModal State.ModalWin
    | OpenHypermodel String
    | AddModel State.Model
    | ChangeTitle String
    | ShowFillInputsDialog
    | ChangeDescription String
    | ModelSearch ModelSearchMsg
    | UIMsg Ports.Msg
    | LoadPage Navigation.Location
    | ExecutionInputs ExecutionInputsMsg
    | PublishHypermodel
    | ShowExperiments
    | ShowRecommendations
    | ExperimentsResponse (Rest.Msg State.Experiments)
