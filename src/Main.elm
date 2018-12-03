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
import TruckViews.TruckFunctions exposing (..)
import Task
import Array exposing(..)
import SearchFilterViews.MakeSearchFilter exposing (..)

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

                yearFilters = buildYearValueTupleList trucks
                makeFilters = buildMakeValueRecordList trucks
                
            in
                ( ({ model | truckList = trucks, filteredTruckList = trucks}, {uiModel | yearFilters = yearFilters, makeFilters = makeFilters}), Cmd.none)
                
        FilterCDLNoCheckBoxClicked userAction ->
            let
                newUIModel = {uiModel | filterCDLNoSelected = userAction}
                currentFilteredTruckList = applySearchFilters model newUIModel
                newSearchFilterList = buildSearchFilters model newUIModel
                -- newFilteredTruckList  =
                --     if userAction then
                --         List.filter (\t -> String.trim t.cdl == "No" ) currentFilteredList
                --     else
                --         currentFilteredList
            in
                ( ({model | filteredTruckList = currentFilteredTruckList}, newUIModel), Cmd.none )

        FilterCDLYesCheckBoxClicked userAction ->
            let
                newUIModel = {uiModel | filterCDLYesSelected = userAction}
                currentFilteredTruckList = applySearchFilters model newUIModel
                -- newFilteredTruckList  =
                --     if userAction then
                --         List.filter (\t -> String.trim t.cdl == "Yes" ) currentFilteredList
                --     else
                --         currentFilteredList
            in
                ( ({model | filteredTruckList = currentFilteredTruckList}, newUIModel), Cmd.none )

        FilterYearCheckBoxClicked index year userAction ->
            let
                newUIModel = 
                    uiModel.yearFilters
                        |> Array.get index
                        |> Maybe.map (\yf -> Tuple.mapSecond (\chkd -> userAction) yf)
                        |> Maybe.map (\yf -> Array.set index yf uiModel.yearFilters)
                        |> Maybe.map (\yfArr -> {uiModel | yearFilters = yfArr})
                        |> Maybe.withDefault uiModel

                -- hasThisTruckYearMatchesWithUserSelectedYear truck = 
                --     newUIModel.yearFilters
                --         |> Array.toList
                --         |> List.filter (\yfModel -> Tuple.first yfModel == truck.year && Tuple.second yfModel == True) 
                --         |> List.length
                --         |> (\length  -> length > 0)
                --     -- List.filter (\x -> Tuple.first x == truck.year && Tuple.second x == True) (Array.toList <| newUIModel.yearFilters )
                --     --     |> List.length
                --     --     |> (\length  -> length > 0)
                --     --List.member truck.year ( List.map (\x -> Tuple.first) (Array.toList <|  newUIModel.yearFilters ) )
                --     --Array.get truck.year  ( Array.map (\x -> Tuple.first x) uiModel.yearFilters )

                -- sortedFilterdTruckList  = 
                --             model.truckList
                --                 |> List.filter (\t -> hasThisTruckYearMatchesWithUserSelectedYear t )
                --                 |> List.sortBy .year
                
                -- yearTruckList  = 
                --             sortedFilterdTruckList
                --                 |> List.length
                --                 |> (\sortedFilterdTruckListLength -> if sortedFilterdTruckListLength > 0 then  sortedFilterdTruckList else model.truckList)

                --modfiledYearFilterList = List.map toggle uiModel.yearFilters

                --selectedYear = List.member year ( List.map (\x -> Tuple.first x) uiModel.yearFilters )
                
                --newSelectedYear = Tuple.mapSecond (\x -> userAction) selectedYear

                --newUIModel =  {uiModel | yearFilters = ((year, userAction) ::  uiModel.yearFilters) }

                --newUIModel =  {uiModel | yearFilters = modfiledYearFilterList }
                newFilteredTruckList = applySearchFilters model newUIModel
            in
                ( ( {model | filteredTruckList = newFilteredTruckList } , newUIModel), Cmd.none )

        
        FilterMakeCheckBoxClicked index make resultCount userAction ->
            let
                newUIModel = 
                    uiModel.makeFilters
                        |> Array.get index
                        |> Maybe.map (\mf -> { mf | userAction = userAction} )
                        |> Maybe.map (\mf -> Array.set index mf uiModel.makeFilters)
                        |> Maybe.map (\mfArr -> {uiModel | makeFilters = mfArr})
                        |> Maybe.withDefault uiModel

                newFilteredTruckList = applySearchFilters model newUIModel

            in
                ( ( {model | filteredTruckList = newFilteredTruckList } , newUIModel), Cmd.none )

        SearchString searchString ->
            let
                apuTruckList  = List.filter (\t -> String.toUpper t.apu == String.toUpper searchString ) model.truckList
            in
                ( ({model | filteredTruckList = apuTruckList}, {uiModel | searchString = searchString}), Cmd.none )
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
                column [hf, wf , pde 5 10 0 10, spy 25] -- Search Filter Panel bc 225 225 225, 
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
                    ,column[scrollbarY,hf, wf, spy 20]
                    [
                        if List.length model.truckList > 0 then
                            --(buildYearValueGroups uiModel model.truckList) -- Year Filter Group
                            ( buildMakeValuesGroup model uiModel ) -- Year Filter Group
                        else
                            loaderIconElement

                        -- ,if List.length model.truckList > 0 then
                        --     ( buildCDLValueGroups model uiModel )  -- CDL Filter Group
                        -- else
                        --     loaderIconElement
                            
                        ,if List.length model.truckList > 0 then
                            --(buildYearValueGroups uiModel model.truckList) -- Year Filter Group
                            ( buildYearValueGroups model uiModel ) -- Year Filter Group
                        else
                            none
                    ]
                ]
                ,column[hf, wfp 5,  bwl 0, bc 225 225 225,pd 15 ] -- Trucks List Panel 
                [
                    row[hf, wf, bw 0, hpx 100, pde 10 10 10 0]
                    [ 
                        column[pdl 15, hf, bc 244 66 95][] --
                        ,column[hf, pdl 5, spaceEvenly][
                            el [] <| textValue <| "Selected Filters... ", 
                            el [] <| textValue <| "Total used trucks found : " ++ (String.fromInt <| (List.length model.filteredTruckList))
                        ]
                    ]
                    ,column[hf, wf, scrollbarY, bw 2] [trucksView model.filteredTruckList]
                    ,row[hf, wf, bw 0, hpx 50, pde 10 10 10 10]
                    [ 
                        column[pdl 15, hf][] --, bc 244 66 95
                        ,column[hf, pdl 5, spaceEvenly][
                            -- el [] <| textValue <| "Page nav bar... ", 
                            -- el [] <| textValue <| "Total Used Trucks : " ++ (String.fromInt <| (List.length model.truckList))
                        ]
                    ]                  
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
