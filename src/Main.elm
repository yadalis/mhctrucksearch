module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Commands exposing (..)
--import RemoteData  exposing (..)
import Http exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Element exposing (..)
import Element.Input as Input exposing (..) 
import Element.Font as Font exposing (..) 
import TruckViews.Truck exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Helpers.Utils exposing (..)
import Helpers.TruckFunctions exposing (..)
import Task
import Array exposing(..)

---- INIT ----

type alias OnLoadSearchFilter =
    String

init : OnLoadSearchFilter -> ( (Model, UIModel) , Cmd Msg)
init jsflg =
    ( (initialModel,initalUIModel jsflg)
        , fetchTrucks
    )

---- UPDATE ----

update : Msg -> (Model, UIModel) -> ( (Model, UIModel) , Cmd Msg  )
update msg (model, uiModel) =
    case msg of
        OnFetchTrucks response ->
            let
                trucks = case response of
                            Ok truckList ->
                                    truckList
                            Err err ->
                                    []
            in
                ( ({ model | truckList = trucks, filteredTruckList = trucks}, uiModel), Cmd.none)
        FilterCDLNoCheckBoxClicked userAction ->
            ( (model, {uiModel | filterCDLNoSelected = userAction}), Cmd.none )
        FilterCDLYesCheckBoxClicked userAction ->
            ( (model, {uiModel | filterCDLYesSelected = userAction}), Cmd.none )
        SearchString searchString ->
            ( (model, {uiModel | searchString = searchString}), Cmd.none )
        SearchPressed ->
            ( (model, uiModel), Cmd.none )

---- VIEW ----


view : (Model, UIModel) -> Html Msg
view (model, uiModel) =
        let
            searchStringBtnStyle = 
                        if String.length uiModel.searchString > 0 then 
                            [ bc 226 63 63, fc 250 250 250] 
                        else
                            [ bc 198 201 206, fc 245 245 245]

            loaderIconElement = 
                    if List.length model.truckList > 0 then
                        none
                    else
                        image [hpx 18, bw one, wf, pdl 5, bwb 2, alignTop] {src = "loader.gif", description ="Logo" }  

            searchBtnIcon =
                    if String.length uiModel.searchString > 0 then 
                        image [hpx 32, bw one] {src = "srch_white.ico", description ="Logo" }
                    else
                        image [hpx 32, bw one] {src = "srch_grey.ico", description ="Logo" }
            navBar =
                    row[wf, hpx 75, bc 47 48 49, fc 250 250 250, alpha  0.95]
                    [
                        image [hpx 32, bw one] {src = "https://az832863.vo.msecnd.net/~/media/images/components/pagelogos/mhclogo.png?_=-381616326&h=61", description ="Logo" }
                        ,
                        el [pdl 25] <| textValue "Truck Search - Powerfull-Flexible search app"
                    ] 
        in
        
        layout [hf, pd 0, inFront navBar ] --  inFront navBar is to make menu bar fixed
        <|
            row[hf,wf, pdt 76]
            [
                column [hf, wf , pde 5 10 0 10, bc 225 225 225, spy 25] -- Search Filter Panel
                [
                    row[wf, pdt 15]
                    [ 
                        Input.text [wf, hf]
                        {
                            onChange = SearchString
                            ,text  = uiModel.searchString
                            ,label = labelLeft [] none
                            ,placeholder = Just (Input.placeholder [] (el [] <| textValue "Search trucks"))
                        }
                        ,Input.button ( [ hf, wpx 50, eId "prntEst"] ++ searchStringBtnStyle)
                            { 
                                onPress = if String.length uiModel.searchString > 0 then Just SearchPressed else Nothing
                                ,label = searchBtnIcon
                            }
                    ]
                    ,if List.length model.truckList > 0 then
                        (buildCDLValueGroups uiModel model.truckList)
                    else
                        loaderIconElement
                ]
                ,column[hf, wfp 5, bc 205 205 205, bwl 0, pdl 15 ] -- Trucks List Panel
                [
                    row[wf, bw 0, hpx 100, pde 10 10 10 0][ el [alignTop] <| textValue <| "Selected Filters... "]
                    ,column[wf, scrollbarY] [trucksView model.truckList]                        
                ]
            ]
            
---- PROGRAM ----


main : Program OnLoadSearchFilter (Model,UIModel) Msg
main =
    Browser.element
        { view = view
        , init = init -- to capture flags from JS
        , update = update
        , subscriptions = always Sub.none
        }
