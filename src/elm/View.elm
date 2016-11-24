module View exposing (view, modalWinIds)

import State exposing (..)
import Msg exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onCheck)
import String
import Http
import RemoteData
import Date.Extra exposing (toFormattedString)
import Date


sidebar : State -> Html Msg
sidebar state =
    div [ id "sidebar", class "ui thin left inverted vertical menu sidebar" ]
        [ a [ class "item" ] [ i [ class "home icon" ] [] ]
        , a [ class "item" ] [ i [ class "block layout icon" ] [] ]
        , a [ class "item" ] [ i [ class "block layout icon" ] [] ]
        ]


type alias ButtonConfig a =
    { title : String
    , position : String
    , fitted : Bool
    , icon : String
    , enabled : Bool
    , msg : Maybe a
    }


newBtn : String -> String -> ButtonConfig msg
newBtn title icon =
    { title = title
    , position = "bottom left"
    , fitted = True
    , icon = icon
    , enabled = False
    , msg = Nothing
    }


btnPosition : String -> ButtonConfig msg -> ButtonConfig msg
btnPosition pos c =
    { c | position = pos }


btnIcon : String -> ButtonConfig msg -> ButtonConfig msg
btnIcon iconClass c =
    { c | icon = iconClass }


btnMsg : msg -> ButtonConfig msg -> ButtonConfig msg
btnMsg m c =
    { c | enabled = True, msg = Just m }


btnToButton : ButtonConfig msg -> Html msg
btnToButton c =
    let
        position =
            attribute "data-position" c.position

        tooltip =
            attribute "data-tooltip" c.title

        attributes =
            [ position
            , tooltip
            , class "compact ui button"
            ]

        evtsAttrs =
            case c.msg of
                Nothing ->
                    attributes

                Just m ->
                    (onClick m) :: attributes

        cls =
            [ (if c.fitted then
                "fitted"
               else
                ""
              )
            , (if c.enabled then
                ""
               else
                "disabled"
              )
            , c.icon
            , "icon orange button"
            ]
                |> addClasses
    in
        button evtsAttrs
            [ i [ class cls ] [] ]


isJust : Maybe a -> Bool
isJust a =
    case a of
        Just _ ->
            True

        _ ->
            False


applyWhen : Bool -> (a -> a) -> a -> a
applyWhen b f thing =
    if b then
        f thing
    else
        thing


applyUnless : Bool -> (a -> a) -> a -> a
applyUnless b =
    applyWhen (not b)


toolbar : State -> Html Msg
toolbar state =
    let
        notEmptyCanvas =
            State.isEmptyCanvas state

        hBtn title icon =
            -- btn title icon hasHypermodel Empty
            newBtn title icon |> btnToButton

        aBtn title icon msg =
            newBtn title icon |> btnMsg msg |> btnToButton

        cBtn title icon msg =
            newBtn title icon |> applyWhen notEmptyCanvas (btnMsg msg) |> btnToButton
    in
        div [ class "ui grid" ]
            [ div [ class "row" ]
                [ div [ class "left floated menubar" ]
                    [ -- div [ class "compact ui button toggle" ] [ i [ class "sidebar icon" ] [] ] ,
                      div [ class "ui buttons" ]
                        [ aBtn "Select a hypermodel to load.." "folder open" LoadHypermodels
                        , aBtn "Start a new hypermodel.." "file outline" NewHypermodel
                        , newBtn "Save hypermodel.." "save" |> applyWhen state.needsSaving (btnMsg SaveHypermodel) |> btnToButton
                        , newBtn "Reload current hypermodel" "refresh" |> applyWhen state.needsSaving (btnMsg ReloadHypermodel) |> btnToButton
                        ]
                    , div [ class "ui buttons" ]
                        [ cBtn "Zoom-in" "zoom" ZoomIn
                        , cBtn "Actual Size" "expand" ZoomActualSize
                        , cBtn "Zoom-Out" "zoom out" ZoomOut
                        ]
                    , div [ class "ui buttons" ]
                        [ newBtn "Select a model from the model repository to add.." "database" |> btnMsg LoadModels |> btnToButton
                          -- , hBtn "Add a time-driven iteration" "hourglass half"
                          -- , hBtn "Add a choice construct" "fork"
                          -- , hBtn "Add a block of a branch" "code"
                          -- , hBtn "Add inputs" "sign in"
                          -- , hBtn "Add outputs" "sign out"
                        ]
                    ]
                , div [ class "ui right floated buttons" ]
                    [ text "Stelios" ]
                ]
            ]


addClasses : List String -> String
addClasses l =
    String.join " " l


modalWinIds : { listHypermodels : String, listModels : String, saveHypermodel : String, errorAlert : String }
modalWinIds =
    { listHypermodels = "hmModalWin"
    , listModels = "mModalWin"
    , saveHypermodel = "savehyperModelWin"
    , errorAlert = "errorAlertWin"
    }


viewDate : Date.Date -> String
viewDate dt =
    -- dt |> toFormattedString "EEEE, MMMM d, y 'at' h:mm a"
    dt |> toFormattedString "dd/MM/y hh:mm"


viewHypermodel : State.Hypermodel -> Html Msg
viewHypermodel { id, title, description, version, created, updated } =
    let
        b =
            button [ onClick (OpenHypermodel id), class "ui right floated button" ]
                [ i [ class "ui cloud download icon" ] []
                , text "Load!"
                ]

        versionStr =
            toString version
    in
        div [ class "item" ]
            [ div
                [ class "ui small image"
                , attribute "data-tooltip" id
                , attribute "data-position" "right center"
                ]
                [ img
                    [ src ("/hme2/preview/" ++ id ++ "/" ++ versionStr)
                    , style [ ( "height", "150px" ), ( "width", "150px" ) ]
                    ]
                    []
                ]
            , div [ class "middle aligned content" ]
                [ div [ class "header" ]
                    [ text title ]
                , div [ class "description" ]
                    [ p []
                        [ text description
                        ]
                    ]
                , div [ class "extra" ]
                    [ b
                    , div [ class "ui label" ] [ text "Nephroblastoma" ]
                    , div []
                        [ "Created : "
                            ++ viewDate created
                            ++ " Updated : "
                            ++ viewDate updated
                            ++ " (version: "
                            ++ versionStr
                            ++ ")"
                            |> text
                        ]
                    ]
                ]
            ]


viewErrorAlert : Http.Error -> Html Msg
viewErrorAlert error =
    -- This is a modal window
    let
        modalWin =
            modalWinIds.errorAlert

        message =
            case error of
                Http.BadUrl str ->
                    "Bad url : " ++ str

                Http.Timeout ->
                    "Timeout!"

                Http.NetworkError ->
                    "Network error"

                Http.BadStatus { status } ->
                    "Server returned status : " ++ status.message ++ " (" ++ toString status.code ++ ")"

                Http.BadPayload payload { status } ->
                    let
                        _ =
                            Debug.log "BAD PAyload: " payload
                    in
                        "Server returned bad payload.. Error : " ++ payload
    in
        div [ id modalWin, class "ui modal small" ]
            [ i [ class "ui right floated  cancel close icon", onClick (CloseModal modalWin) ] []
            , div [ class "header" ] [ text "Server Error" ]
            , div [ class "content" ]
                [ text message
                ]
            , div [ class "actions" ]
                [ div [ class "ui cancel button", onClick (CloseModal modalWin) ] [ text "Cancel" ] ]
            ]


viewHypermodels : List State.Hypermodel -> Html Msg
viewHypermodels allHypermodels =
    -- This is a modal window
    let
        sortedHypermodels =
            List.sortBy (.updated >> Date.toTime >> negate) allHypermodels
    in
        div [ id modalWinIds.listHypermodels, class "ui modal long scrolling" ]
            [ i [ class "ui right floated  cancel close icon", onClick (CloseModal modalWinIds.listHypermodels) ] []
            , div [ class "header" ] [ text "Available Hypermodels" ]
            , div [ class "content", style [ ( "height", "400px" ), ( "overflow-x", "scroll" ) ] ]
                [ div [ class "ui items" ]
                    (List.map viewHypermodel sortedHypermodels)
                ]
            , div [ class "actions" ]
                [ div [ class "ui cancel button", onClick (CloseModal modalWinIds.listHypermodels) ] [ text "Cancel" ] ]
            ]


viewModel : State.State -> State.Model -> Html Msg
viewModel state m =
    let
        isUsed =
            State.modelIsUsed state m

        isStronglyCoupled =
            State.modelIsDynamic m

        styles =
            (if isStronglyCoupled then
                [ ( "font-weight", "bold" ), ( "font-style", "italic" ) ]
             else
                []
            )
                ++ (if isUsed then
                        [ ( "text-decoration", "line-through" ) ]
                    else
                        []
                   )

        b =
            newBtn ("Add " ++ m.title) "Plus" |> btnPosition "left center" |> btnMsg (AddModel m) |> btnToButton
    in
        tr
            []
            [ td [ style styles, classList [ ( "disabled", isUsed ), ( "collapsing", True ) ] ] [ text m.title ]
            , td [ style styles, classList [ ( "disabled", isUsed ) ] ] [ text m.description ]
            , td [ class "collapsing" ] [ b ]
            ]


viewModels : State.State -> State.ModelSearchState -> Html Msg
viewModels state modelSearch =
    -- This is a modal window
    let
        modelsList =
            RemoteData.withDefault [] state.allModels

        models0 =
            if modelSearch.stronglyCoupledOnly then
                List.filter State.modelIsDynamic modelsList
            else
                modelsList

        models1 =
            if modelSearch.frozenOnly then
                List.filter .frozen models0
            else
                models0

        models2 =
            case modelSearch.title of
                Nothing ->
                    models1

                Just str ->
                    let
                        strU =
                            String.toUpper str
                    in
                        List.filter (.title >> String.toUpper >> String.contains strU) models1

        titleSearch =
            case modelSearch.title of
                Nothing ->
                    ""

                Just str ->
                    str
    in
        div [ id modalWinIds.listModels, class "ui modal long scrolling" ]
            [ i [ class "ui right floated  cancel close icon", onClick (CloseModal modalWinIds.listModels) ] []
            , div [ class "header" ]
                [ div []
                    [ text "Available Models"
                    , div [ class "floating ui teal label" ]
                        [ text (models2 |> List.length |> toString)
                        ]
                    ]
                ]
            , div [ class "content" ]
                [ Html.form [ class "ui form" ]
                    [ div [ class "field" ]
                        [ div [ class "ui input" ]
                            [ input [ type_ "text", placeholder "filter by name", value titleSearch, onInput ModelSearchTitle ] []
                            ]
                        ]
                    , div [ class "field" ]
                        [ div [ class "ui checkbox" ]
                            [ input
                                [ type_ "checkbox"
                                , checked modelSearch.frozenOnly
                                , onCheck ModelSearchFrozen
                                ]
                                []
                            , label []
                                [ text "Stable only" ]
                            ]
                        ]
                    , div [ class "field" ]
                        [ div [ class "ui checkbox" ]
                            [ input
                                [ type_ "checkbox"
                                , checked modelSearch.stronglyCoupledOnly
                                , onCheck ModelSearchStronglyCoupled
                                ]
                                []
                            , label []
                                [ text "Strongly coupled only" ]
                            ]
                        ]
                    , div [ class "field" ]
                        [ label []
                            [ text "Perspective 1" ]
                        , select [ class "ui dropdown" ]
                            [ option [ value "" ]
                                [ text "Gender" ]
                            , option [ value "1" ]
                                [ text "Male" ]
                            , option [ value "0" ]
                                [ text "Female" ]
                            ]
                        ]
                    ]
                , div [ style [ ( "height", "400px" ), ( "overflow-x", "scroll" ) ] ]
                    [ table [ class "ui small celled striped padded table" ]
                        [ thead [] [ tr [] [ th [] [ text "Title" ], th [] [ text "Description" ], th [] [ text "Open?" ] ] ]
                        , tbody [] (List.map (viewModel state) models2)
                        ]
                    ]
                ]
            , div [ class "actions" ]
                [ div [ class "ui cancel button", onClick (CloseModal modalWinIds.listModels) ] [ text "Cancel" ] ]
            ]


viewSaveHypermodel : State.Hypermodel -> Html Msg
viewSaveHypermodel hm =
    -- This is a modal window
    let
        modalId =
            modalWinIds.saveHypermodel
    in
        div [ id modalId, class "ui modal" ]
            [ i [ class "ui right floated cancel close icon", onClick (CloseModal modalId) ] []
            , div [ class "header" ] [ text hm.title ]
            , div [ class "content" ]
                [ Html.form [ class "ui form" ]
                    [ div [ class "field" ]
                        [ text "ID:"
                        , code
                            []
                            [ hm.id |> text ]
                        ]
                    , div
                        [ class "field" ]
                        [ label [] [ text "Name" ]
                        , input [ onInput ChangeTitle, value hm.title ] []
                        ]
                    , div [ class "field" ]
                        [ label [] [ text "Description" ]
                        , textarea [ onInput ChangeDescription, value hm.description ] []
                        ]
                    ]
                ]
            , div [ class "actions" ]
                [ div [ class "ui primary button", onClick DoSaveHypermodel ] [ text "Save" ]
                , div [ class "ui cancel button", onClick (CloseModal modalId) ] [ text "Cancel" ]
                ]
            ]


view : State -> Html Msg
view state =
    let
        title =
            "CHIC Hypermodeling Editor"
                ++ (if String.isEmpty state.wip.title then
                        ""
                    else
                        ": " ++ state.wip.title
                   )
                |> applyWhen (Debug.log "DIRTY:" state.needsSaving) (\tt -> String.append tt " *")

        loading =
            state.pendingRestCalls > 0 || RemoteData.isLoading state.allModels

        loaderClasses =
            (if loading then
                "active"
             else
                "disabled"
            )
                :: [ "ui", "indeterminate", "text", "loader" ]
                |> addClasses
    in
        div [ class "ui" ]
            [ sidebar state
            , div [ class "pusher" ]
                [ h2 [ class "title" ] [ text title ]
                , toolbar state
                ]
            , div [ class loaderClasses ] [ text state.busyMessage ]
            , viewHypermodels state.allHypermodels
            , viewModels state state.modelSearch
            , viewSaveHypermodel state.wip
            , case state.serverError of
                Nothing ->
                    div [] []

                Just error ->
                    viewErrorAlert error
            ]
