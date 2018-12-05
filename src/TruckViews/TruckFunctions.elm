module TruckViews.TruckFunctions exposing (..)

import Element exposing (..)
import Element.Input exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Helpers.Utils exposing (..)
import Model exposing (..)
import Msg exposing (..)
import List.Unique exposing (..)
import Array exposing (..)

applySearchFilters: Model -> UIModel -> List Truck
applySearchFilters model uiModel =
    let
        hasThisTruckYearMatchesWithUserSelectedYear truck = 
            uiModel.yearFilters
                |> Array.filter (\sf -> Tuple.first sf == truck.year && Tuple.second sf == True)
                |> Array.length
                |> (\length  -> length > 0)

        hasThisTruckMakeMatchesWithUserSelectedMake truck = 
            uiModel.makeFilters
                |> Array.filter (\sf -> String.trim sf.searchFilterKey == String.trim truck.make && sf.userAction == True) 
                |> Array.length
                |> (\length  -> length > 0)
        
        hasThisTruckModelMatchesWithUserSelectedModel truck = 
            uiModel.modelFilters
                |> Array.filter (\sf -> String.trim sf.searchFilterKey == String.trim truck.model && sf.userAction == True) 
                |> Array.length
                |> (\length  -> length > 0)

        hasThisTruckSalesStatusMatchesWithUserSelectedSalesStatus truck = 
            uiModel.salesStatusFilters
                |> Array.filter (\sf -> String.trim sf.searchFilterKey == String.trim truck.salesStatus && sf.userAction == True) 
                |> Array.length
                |> (\length  -> length > 0)

        hasThisTruckSleeperRoofMatchesWithUserSelectedSleeperRoof truck = 
            uiModel.sleeperRoofFilters
                |> Array.filter (\sf -> String.trim sf.searchFilterKey == String.trim truck.sleeperRoof && sf.userAction == True) 
                |> Array.length
                |> (\length  -> length > 0)

        hasThisTruckSleeperBunkMatchesWithUserSelectedSleeperBunk truck = 
            uiModel.sleeperBunkFilters
                |> Array.filter (\sf -> String.trim sf.searchFilterKey == String.trim truck.sleeperBunk && sf.userAction == True) 
                |> Array.length
                |> (\length  -> length > 0)

        hasSearchFilterValuesChecked searchFilters =
            searchFilters
                |> Array.filter (\sf -> sf.userAction == True) 
                |> Array.length
                |> (\length  -> length > 0)

        hasYearSearchFilterValuesChecked  =
            uiModel.yearFilters
                |> Array.filter (\sf -> Tuple.second sf == True)
                |> Array.length
                |> (\length  -> length > 0)

        filterdTruckList  = 
            model.truckList
                |> (\trks ->
                        if hasSearchFilterValuesChecked uiModel.salesStatusFilters then
                            List.filter (\t -> hasThisTruckSalesStatusMatchesWithUserSelectedSalesStatus t ) trks 
                        else
                            trks
                    )
                |> (\trks ->
                        if hasYearSearchFilterValuesChecked then
                            List.filter (\t -> hasThisTruckYearMatchesWithUserSelectedYear t ) trks 
                        else
                            trks
                    )
                |> (\trks ->
                        if hasSearchFilterValuesChecked uiModel.makeFilters then
                            List.filter (\t -> hasThisTruckMakeMatchesWithUserSelectedMake t ) trks 
                        else
                            trks
                    )
                |> (\trks ->
                        if hasSearchFilterValuesChecked uiModel.modelFilters then
                            List.filter (\t -> hasThisTruckModelMatchesWithUserSelectedModel t ) trks 
                        else
                            trks
                    )
                |> (\trks ->
                        if hasSearchFilterValuesChecked uiModel.sleeperRoofFilters then
                            List.filter (\t -> hasThisTruckSleeperRoofMatchesWithUserSelectedSleeperRoof t ) trks 
                        else
                            trks
                    )
                |> (\trks ->
                        if hasSearchFilterValuesChecked uiModel.sleeperBunkFilters then
                            List.filter (\t -> hasThisTruckSleeperBunkMatchesWithUserSelectedSleeperBunk t ) trks 
                        else
                            trks
                    )
        
        sortedFilterdTruckList =
            filterdTruckList
                |> List.sortBy .make
    in
        sortedFilterdTruckList

----------------------------------------------------------------------------------------------------------------------------------------


--flippedComparison a b =
desendingOrder a b =
    case compare a b of
        LT -> GT
        EQ -> EQ
        GT -> LT
        
buildYearValueList : List Truck -> Array Int
buildYearValueList trucks =
    List.map (\t -> t.year) trucks
        |> filterDuplicates
        |> List.sortWith desendingOrder -- to do descending order
        |> Array.fromList

buildYearValueTupleList : List Truck -> Array (Int, Bool)
buildYearValueTupleList trucks =
    buildYearValueList trucks
        |> Array.map (\year -> (year, False))

buildYearValueGroups : Model -> UIModel -> Element Msg
buildYearValueGroups model uiModel = --currentFilteredTrucks =
    let
        yearFilters = uiModel.yearFilters

        buildYearCheckboxes :  Int -> (Int, Bool) -> Element Msg
        buildYearCheckboxes index (year, userAction) =
            let
                yearWiseCount =    List.filter (\t -> t.year == year) model.filteredTruckList --currentFilteredTrucks
            in
                row[bw two]
                [
                    checkbox [bw one, pdr 5 ] {
                        onChange = FilterYearCheckBoxClicked index year
                        ,icon = buildChkBoxImage
                        , label = labelRight [] (el [] <| textValue (String.fromInt year) )
                        --, checked = uiModel.filterSelectionsModel.filterCDLNoSelected
                        , checked = userAction
                    }
                    , textValue <| " (" ++  (String.fromInt <| (List.length yearWiseCount))  ++ ")"
                ]
                -- if List.length yearWiseCount > 0 then
                --     row[bw two]
                --     [
                --         checkbox [bw one, pdr 5 ] {
                --             onChange = FilterYearCheckBoxClicked index year
                --             ,icon = buildChkBoxImage
                --             , label = labelRight [] (el [] <| textValue (String.fromInt year) )
                --             --, checked = uiModel.filterSelectionsModel.filterCDLNoSelected
                --             , checked = userAction
                --         }
                --         , textValue <| " (" ++  (String.fromInt <| (List.length yearWiseCount))  ++ ")"
                --     ]
                -- else
                --     none
    in
        row[spy 15, wf]
        [
            column[spy 10, wf,  bw one]
            [
                row[bw 0, hf, bwb 1, wf, pdb 3]
                [
                    paragraph [bw one, fal, wf, bc 200 200 200, hpx 25, pd 5][textValue <| "Year"]
                ]
                ,column[spy 10, pdl 15, hf, scrollbarY, wf]
                (
                    Array.toList <| Array.indexedMap buildYearCheckboxes yearFilters -- column function needs List of item and not Array of items, so need conversion
                )
            ]
        ]

buildChkBoxImage userAction =
        if userAction == True then 
            image [hpx 24] {src = "checked.png", description ="Logo" }
        else 
            el [hpx 24, wpx 24, bw 2, br 5] <| none