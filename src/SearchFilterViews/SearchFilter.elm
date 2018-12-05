
module SearchFilterViews.SearchFilter exposing (..)

import Element exposing (..)
import Element.Input exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Helpers.Utils exposing (..)
import Model exposing (..)
import Msg exposing (..)
import List.Unique exposing (..)
import Array exposing (..)
 
--flippedComparison a b =
desendingOrder a b =
    case compare a b of
        LT -> GT
        EQ -> EQ
        GT -> LT

filterEmptyValuesFromList : List String -> List String
filterEmptyValuesFromList  searchFilterList =
    List.filter (
                    \str -> 
                        str
                            |> String.trim
                            |> String.isEmpty
                            |> not 
                )
                
                searchFilterList

applyExtraOnSearchFilter  : List String -> Array String
applyExtraOnSearchFilter searchFilterKeyValue =
    filterDuplicates searchFilterKeyValue
        |> filterEmptyValuesFromList
        |> List.sort
        |> Array.fromList

buildSearchFilterValueList : SearchFilterCustomType -> List Truck -> Array String
buildSearchFilterValueList searchFilterCustomType trucks =
    case searchFilterCustomType of
        SalesStatus -> 
            List.map (\t -> t.salesStatus) trucks
                |> applyExtraOnSearchFilter

        Make -> 
            List.map (\t -> t.make) trucks
                |> applyExtraOnSearchFilter

        MakeModel -> 
            List.map (\t -> t.model) trucks
                |> applyExtraOnSearchFilter

        SleeperRoof -> 
            List.map (\t -> t.sleeperRoof) trucks
                |> applyExtraOnSearchFilter
        SleeperBunk -> 
            List.map (\t -> t.sleeperBunk) trucks
                |> applyExtraOnSearchFilter

buildSearchFilterValueRecordList : SearchFilterCustomType -> List Truck -> Array SearchFilterType
buildSearchFilterValueRecordList searchFilterCustomType trucks =
    buildSearchFilterValueList searchFilterCustomType trucks
        |> Array.map (\sfValue -> {searchFilterKey = sfValue, userAction = False, resultCount = 0})

buildSearchFilterValuesGroup : SearchFilterCustomType ->  Model -> UIModel -> Element Msg
buildSearchFilterValuesGroup searchFilterCustomType model uiModel =
    let
            (searchFilters, filterLabel, msg)
                =   case searchFilterCustomType of
                            SalesStatus -> 
                                (uiModel.salesStatusFilters, "Sales Status", FilterSalesStatusCheckBoxClicked)
                            
                            Make -> 
                                (uiModel.makeFilters, "Make", FilterMakeCheckBoxClicked)

                            MakeModel -> 
                                (uiModel.modelFilters, "Model", FilterModelCheckBoxClicked)

                            SleeperRoof -> 
                                (uiModel.sleeperRoofFilters, "Sleeper Roof", FilterSleeperRoofCheckBoxClicked)
                                
                            SleeperBunk -> 
                                (uiModel.sleeperBunkFilters, "Sleeper Bunk", FilterSleeperBunkCheckBoxClicked)

            buildCheckboxes :  Int -> SearchFilterType -> Element Msg
            buildCheckboxes index searchFilter =
                let
                    searchKeyWiseCount =
                         case searchFilterCustomType of
                            SalesStatus -> 
                                List.filter (\t -> String.trim t.salesStatus == searchFilter.searchFilterKey) model.filteredTruckList
                            Make -> 
                                List.filter (\t -> String.trim t.make == searchFilter.searchFilterKey) model.filteredTruckList
                            MakeModel -> 
                                List.filter (\t -> String.trim t.model == searchFilter.searchFilterKey) model.filteredTruckList
                            SleeperRoof -> 
                                List.filter (\t -> String.trim t.sleeperRoof == searchFilter.searchFilterKey) model.filteredTruckList
                            SleeperBunk -> 
                                List.filter (\t -> String.trim t.sleeperBunk == searchFilter.searchFilterKey) model.filteredTruckList
                in
                    row[bw two]
                    [
                        checkbox [bw one, pdr 5 ] {
                            onChange = msg index
                            ,icon = buildChkBoxImage
                            , label = labelRight [] (el [] <| textValue searchFilter.searchFilterKey )
                            , checked = searchFilter.userAction
                        }
                        , textValue <| " (" ++  (String.fromInt <| (List.length searchKeyWiseCount))  ++ ")"
                    ]
    in
        row[spy 15, wf]
        [
            column[spy 10, wf,  bw one]
            [
                row[bw 0, hf, bwb 1, wf, pdb 3]
                [
                    paragraph [bw one, fal, wf, bc 200 200 200, hpx 25, pd 5, centerY][textValue <| filterLabel]
                ]
                ,column[spy 10, pdl 15, hf, scrollbarY, wf]
                (
                    Array.toList <| Array.indexedMap buildCheckboxes searchFilters -- column function needs List of item and not Array of items, so need conversion
                )
            ]
        ]

buildChkBoxImage userAction =
        if userAction == True then 
            image [hpx 24] {src = "checked.png", description ="Logo" }
        else 
            el [hpx 24, wpx 24, bw 2, br 5] <| none