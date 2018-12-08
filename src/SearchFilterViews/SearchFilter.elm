
module SearchFilterViews.SearchFilter exposing (..)

import Element exposing (..)
import Element.Input exposing (..)
import Element.Font exposing (..)
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

applyExtraOnSearchFilter  : Int -> List String -> Array String
applyExtraOnSearchFilter sortOrder searchFilterKeyValue =
    filterDuplicates searchFilterKeyValue
        |> filterEmptyValuesFromList
        |> (if sortOrder == 0 then 
                List.sort 
            else 
                List.sortWith desendingOrder)
        |> Array.fromList

buildSearchFilterValueList : SearchFilterCustomType -> List Truck -> Array (String, Int)
buildSearchFilterValueList searchFilterCustomType trucks =
    case searchFilterCustomType of
        SalesStatus -> 
            List.map (\t -> t.salesStatus) trucks
                |> applyExtraOnSearchFilter 0
                |> (\sfArray -> 
                                Array.map (\sf -> 
                                                Tuple.pair sf (List.length <| (List.filter (\t -> String.trim t.salesStatus == sf) trucks )) ) sfArray
                    ) 

        Year -> 
            List.map (\t -> t.year) trucks
                |> applyExtraOnSearchFilter 1
                |> (\sfArray -> 
                                Array.map (\sf -> 
                                                Tuple.pair sf (List.length <| (List.filter (\t -> String.trim t.year == sf) trucks )) ) sfArray
                    )
                
        Make -> 
            List.map (\t -> t.make) trucks
                |> applyExtraOnSearchFilter 0
                |> (\sfArray -> 
                                Array.map (\sf -> 
                                                Tuple.pair sf (List.length <| (List.filter (\t -> String.trim t.make == sf) trucks )) ) sfArray
                    )                

        MakeModel -> 
            List.map (\t -> t.model) trucks
                |> applyExtraOnSearchFilter 0
                |> (\sfArray -> 
                                Array.map (\sf -> 
                                                Tuple.pair sf (List.length <| (List.filter (\t -> String.trim t.model == sf) trucks )) ) sfArray
                    )                

        SleeperRoof -> 
            List.map (\t -> t.sleeperRoof) trucks
                |> applyExtraOnSearchFilter 0
                |> (\sfArray -> 
                                Array.map (\sf -> 
                                                Tuple.pair sf (List.length <| (List.filter (\t -> String.trim t.sleeperRoof == sf) trucks )) ) sfArray
                    )                
                
        SleeperBunk -> 
            List.map (\t -> t.sleeperBunk) trucks
                |> applyExtraOnSearchFilter 0
                |> (\sfArray -> 
                                Array.map (\sf -> 
                                                Tuple.pair sf (List.length <| (List.filter (\t -> String.trim t.sleeperBunk == sf) trucks )) ) sfArray
                    )                

buildSearchFilterValueRecordList : SearchFilterCustomType -> List Truck -> Array SearchFilterType
buildSearchFilterValueRecordList searchFilterCustomType trucks =
    buildSearchFilterValueList searchFilterCustomType trucks
        |> Array.indexedMap (\index  sfValue -> {index = index, searchFilterKey = Tuple.first sfValue, userAction = False, resultCount = Tuple.second sfValue, filterCategory = searchFilterCustomType})

buildSearchFilterValuesGroup : SearchFilterCustomType ->  Model -> UIModel -> Element Msg
buildSearchFilterValuesGroup searchFilterCustomType model uiModel =
    let
            (searchFilters, filterLabel, msg)
                =   case searchFilterCustomType of
                            SalesStatus -> 
                                (uiModel.salesStatusFilters, "Sales Status", FilterCheckBoxClicked )

                            Year -> 
                                (uiModel.yearFilters, "Year", FilterCheckBoxClicked)

                            Make -> 
                                (uiModel.makeFilters, "Make", FilterCheckBoxClicked)

                            MakeModel -> 
                                (uiModel.modelFilters, "Model", FilterCheckBoxClicked)

                            SleeperRoof -> 
                                (uiModel.sleeperRoofFilters, "Sleeper Roof", FilterCheckBoxClicked)
                                
                            SleeperBunk -> 
                                (uiModel.sleeperBunkFilters, "Sleeper Bunk", FilterCheckBoxClicked)

            buildCheckboxes :  Int -> SearchFilterType -> Element Msg
            buildCheckboxes index searchFilter =
                    row[bw two, size 14]
                    [
                        checkbox [bw one, pdr 0 ] {
                            onChange = msg index searchFilterCustomType
                            ,icon = buildChkBoxImage
                            , label = labelRight [centerY] (el [] <| textValue searchFilter.searchFilterKey )
                            , checked = searchFilter.userAction
                        }
                        , textValue <| " (" ++  (String.fromInt <| searchFilter.resultCount)  ++ ")"
                    ]
    in
        row[spy 15, wf]
        [
            column[spy 10, wf,  bw one]
            [
                row[bw 0, hf, bwb 1, wf, pdb 1]
                [
                    paragraph [bw one, fal, wf, bc 221 221 221, hpx 25, pd 5, centerY][textValue <| filterLabel]
                   
                ]
                ,column[spy 10, pdl 15, scrollbarY, wf, expandCollapseAll uiModel.expandCollapseAllChecked]
                (
                    Array.toList <| Array.indexedMap buildCheckboxes searchFilters -- column function needs List of item and not Array of items, so need conversion
                )
            ]
        ]

expandCollapseAll state =
    
        if state then
            hf
        else
            hpx 0

buildChkBoxImage userAction =
        if userAction == True then 
            image [hpx 16] {src = "checked.png", description ="Logo" }
        else 
            el [hpx 16, wpx 16, bw 1, br 3] <| none --this put empty square, unchecked checkbox style