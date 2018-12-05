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
import Element.Border as Border exposing (..) 
import Element.Font as Font exposing (..) 
import TruckViews.Truck exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Helpers.Utils exposing (..)
import BusinessFunctions.TruckFunctions exposing (..)
import Task
import Array exposing(..)

import SearchFilterViews.SearchFilter exposing (..)

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

                salesStatusFilters = buildSearchFilterValueRecordList SalesStatus trucks
                yearFilters = buildSearchFilterValueRecordList Year trucks
                makeFilters = buildSearchFilterValueRecordList Make trucks
                modelFilters = buildSearchFilterValueRecordList MakeModel trucks
                sleeperRoofFilters = buildSearchFilterValueRecordList SleeperRoof trucks
                sleeperBunkFilters = buildSearchFilterValueRecordList SleeperBunk trucks
            in
                ( (
                        { model | truckList = trucks, filteredTruckList = trucks},
                        {uiModel | 
                                    yearFilters = yearFilters, 
                                    makeFilters = makeFilters, 
                                    modelFilters = modelFilters, 
                                    salesStatusFilters = salesStatusFilters, 
                                    sleeperRoofFilters = sleeperRoofFilters, 
                                    sleeperBunkFilters = sleeperBunkFilters }), Cmd.none)
        
        -- FilterYearCheckBoxClicked index userAction ->
        --     let
        --         newUIModel = 
        --             uiModel.yearFilters
        --                 |> Array.get index
        --                 --|> Maybe.map (\yf -> Tuple.mapSecond (\chkd -> userAction) yf)
        --                 |> Maybe.map (\mf -> { mf | userAction = userAction} )
        --                 |> Maybe.map (\yf -> Array.set index yf uiModel.yearFilters)
        --                 |> Maybe.map (\yfArr -> {uiModel | yearFilters = yfArr})
        --                 |> Maybe.withDefault uiModel

        --         newFilteredTruckList = applySearchFilters model newUIModel
        --     in
        --         ( ( {model | filteredTruckList = newFilteredTruckList } , newUIModel), Cmd.none )
        
        -- FilterMakeCheckBoxClicked index userAction->
        --     let
        --         newUIModel = 
        --             uiModel.makeFilters
        --                 |> Array.get index
        --                 |> Maybe.map (\mf -> { mf | userAction = userAction} )
        --                 |> Maybe.map (\mf -> Array.set index mf uiModel.makeFilters)
        --                 |> Maybe.map (\mfArr -> {uiModel | makeFilters = mfArr})
        --                 |> Maybe.withDefault uiModel

        --         newFilteredTruckList = applySearchFilters model newUIModel

        --     in
        --         ( ( {model | filteredTruckList = newFilteredTruckList } , newUIModel), Cmd.none )
        
        -- FilterModelCheckBoxClicked index userAction ->
        --     let
        --         newUIModel = 
        --             uiModel.modelFilters
        --                 |> Array.get index
        --                 |> Maybe.map (\mf -> { mf | userAction = userAction} )
        --                 |> Maybe.map (\mf -> Array.set index mf uiModel.modelFilters)
        --                 |> Maybe.map (\mfArr -> {uiModel | modelFilters = mfArr})
        --                 |> Maybe.withDefault uiModel

        --         newFilteredTruckList = applySearchFilters model newUIModel

        --     in
        --         ( ( {model | filteredTruckList = newFilteredTruckList } , newUIModel), Cmd.none )


        FilterCheckBoxClicked index searchFilterCustomType userAction ->
            let
                newUIModel = 
                    case searchFilterCustomType of
                        SalesStatus -> 
                            uiModel.salesStatusFilters
                                |> Array.get index
                                |> Maybe.map (\mf -> { mf | userAction = userAction} )
                                |> Maybe.map (\mf -> Array.set index mf uiModel.salesStatusFilters)
                                |> Maybe.map (\mfArr -> {uiModel | salesStatusFilters = mfArr})
                                |> Maybe.withDefault uiModel

                        Year -> 
                             uiModel.yearFilters
                                |> Array.get index
                                --|> Maybe.map (\yf -> Tuple.mapSecond (\chkd -> userAction) yf)
                                |> Maybe.map (\mf -> { mf | userAction = userAction} )
                                |> Maybe.map (\yf -> Array.set index yf uiModel.yearFilters)
                                |> Maybe.map (\yfArr -> {uiModel | yearFilters = yfArr})
                                |> Maybe.withDefault uiModel
                                
                        Make -> 
                             uiModel.makeFilters
                                |> Array.get index
                                |> Maybe.map (\mf -> { mf | userAction = userAction} )
                                |> Maybe.map (\mf -> Array.set index mf uiModel.makeFilters)
                                |> Maybe.map (\mfArr -> {uiModel | makeFilters = mfArr})
                                |> Maybe.withDefault uiModel

                        MakeModel -> 
                            uiModel.modelFilters
                                |> Array.get index
                                |> Maybe.map (\mf -> { mf | userAction = userAction} )
                                |> Maybe.map (\mf -> Array.set index mf uiModel.modelFilters)
                                |> Maybe.map (\mfArr -> {uiModel | modelFilters = mfArr})
                                |> Maybe.withDefault uiModel

                        SleeperRoof -> 
                            uiModel.sleeperRoofFilters
                                |> Array.get index
                                |> Maybe.map (\mf -> { mf | userAction = userAction} )
                                |> Maybe.map (\mf -> Array.set index mf uiModel.sleeperRoofFilters)
                                |> Maybe.map (\mfArr -> {uiModel | sleeperRoofFilters = mfArr})
                                |> Maybe.withDefault uiModel
                                
                        SleeperBunk -> 
                            uiModel.sleeperBunkFilters
                                |> Array.get index
                                |> Maybe.map (\mf -> { mf | userAction = userAction} )
                                |> Maybe.map (\mf -> Array.set index mf uiModel.sleeperBunkFilters)
                                |> Maybe.map (\mfArr -> {uiModel | sleeperBunkFilters = mfArr})
                                |> Maybe.withDefault uiModel

                newFilteredTruckList = applySearchFilters model newUIModel  --SearchFilterCustomType -> Model -> Array SearchFilterType -> List Truck

            in
                ( ( {model | filteredTruckList = newFilteredTruckList } , newUIModel), Cmd.none )


        -- FilterSalesStatusCheckBoxClicked index userAction ->
        --     let
        --         newUIModel = 
        --             uiModel.salesStatusFilters
        --                 |> Array.get index
        --                 |> Maybe.map (\mf -> { mf | userAction = userAction} )
        --                 |> Maybe.map (\mf -> Array.set index mf uiModel.salesStatusFilters)
        --                 |> Maybe.map (\mfArr -> {uiModel | salesStatusFilters = mfArr})
        --                 |> Maybe.withDefault uiModel

        --         newFilteredTruckList = applySearchFilters model newUIModel  --SearchFilterCustomType -> Model -> Array SearchFilterType -> List Truck

        --     in
        --         ( ( {model | filteredTruckList = newFilteredTruckList } , newUIModel), Cmd.none )

        -- FilterSleeperRoofCheckBoxClicked index userAction ->
        --     let
        --         newUIModel = 
        --             uiModel.sleeperRoofFilters
        --                 |> Array.get index
        --                 |> Maybe.map (\mf -> { mf | userAction = userAction} )
        --                 |> Maybe.map (\mf -> Array.set index mf uiModel.sleeperRoofFilters)
        --                 |> Maybe.map (\mfArr -> {uiModel | sleeperRoofFilters = mfArr})
        --                 |> Maybe.withDefault uiModel

        --         newFilteredTruckList = applySearchFilters model newUIModel

        --     in
        --         ( ( {model | filteredTruckList = newFilteredTruckList } , newUIModel), Cmd.none )

        -- FilterSleeperBunkCheckBoxClicked index userAction ->
        --     let
        --         newUIModel = 
        --             uiModel.sleeperBunkFilters
        --                 |> Array.get index
        --                 |> Maybe.map (\mf -> { mf | userAction = userAction} )
        --                 |> Maybe.map (\mf -> Array.set index mf uiModel.sleeperBunkFilters)
        --                 |> Maybe.map (\mfArr -> {uiModel | sleeperBunkFilters = mfArr})
        --                 |> Maybe.withDefault uiModel

        --         newFilteredTruckList = applySearchFilters model newUIModel

        --     in
        --         ( ( {model | filteredTruckList = newFilteredTruckList } , newUIModel), Cmd.none )

        SearchString searchString ->
            let
                apuTruckList  = List.filter (\t -> String.toUpper t.apu == String.toUpper searchString ) model.truckList
            in
                ( ({model | filteredTruckList = apuTruckList}, {uiModel | searchString = searchString}), Cmd.none )
        SearchPressed ->
            let
                newFilteredTruckList = applySearchFilters model uiModel
            in
                ( ( {model | filteredTruckList = newFilteredTruckList } , uiModel), Cmd.none )

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
                        
            focusStyle : Element.Option
            focusStyle =
                Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
        
            navBar =
                    row[wf, hpx 75, bc 47 48 49, fc 250 250 250, alpha  0.95]
                    [
                        image [hpx 32, bw one] {src = "https://az832863.vo.msecnd.net/~/media/images/components/pagelogos/mhclogo.png?_=-381616326&h=61", description ="Logo" }
                        ,
                        el [pdl 25] <| textValue "Truck Search - Powerfull-Flexible search app"
                    ] 
        in
        
        layoutWith {options = [focusStyle]}  [hf, pd 0, inFront navBar ] --  inFront navBar is to make menu bar fixed
        <|
            row[hf,wf, pdt 76]
            [
                column [hf, wf , pde 5 10 0 10, spy 25] -- Search Filter Panel bc 225 225 225, 
                [
                    row[wf, pd 5, bw 1, spaceEvenly]
                    [ 
                        Input.text [wf, hf, bw 0]
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
                    ,column[scrollbarY,hf, wf, spy 20]
                    [
                        if List.length model.truckList > 0 then
                            (buildSearchFilterValuesGroup SalesStatus model uiModel)
                        else
                            none
                        ,if List.length model.truckList > 0 then
                            ( buildSearchFilterValuesGroup Year model uiModel)
                        else
                            none
                        ,if List.length model.truckList > 0 then
                            ( buildSearchFilterValuesGroup Make model uiModel )
                        else
                            loaderIconElement    
                        , if List.length model.truckList > 0 then
                            (buildSearchFilterValuesGroup MakeModel model uiModel)
                        else
                            none
                        , if List.length model.truckList > 0 then
                            (buildSearchFilterValuesGroup SleeperRoof model uiModel)
                        else
                            none
                        , if List.length model.truckList > 0 then
                            (buildSearchFilterValuesGroup SleeperBunk model uiModel)
                        else
                            none                                                        

                    ]
                ]
                ,column[hf, wfp 5,  bwl 0, bc 225 225 225,pd 15 ] -- Trucks Search Result List Panel 
                [
                    row[hf, wf, bw 0, hpx 75, pde 10 10 10 0]
                    [ 
                        column[pdl 15, hf, bc 244 66 95][] --
                        ,column[hf, pdl 5, spaceEvenly][
                            el [] <| textValue <| "Selected Filters... ", 
                            el [] <| textValue <| "Total used trucks found : " ++ (String.fromInt <| (List.length model.filteredTruckList))
                        ]
                    ]
                    ,column[hf, wf, scrollbarY, bw 2] [trucksView model.filteredTruckList]
                    -- ,row[hf, wf, bw 0, hpx 50, pde 10 10 10 10]
                    -- [ 
                    --     column[pdl 15, hf][] --, bc 244 66 95
                    --     ,column[hf, pdl 5, spaceEvenly][
                    --         -- el [] <| textValue <| "Page nav bar... ", 
                    --         -- el [] <| textValue <| "Total Used Trucks : " ++ (String.fromInt <| (List.length model.truckList))
                    --     ]
                    -- ]                  
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
