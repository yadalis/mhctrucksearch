
module SearchFilterViews.SearchFilterRage exposing (..)

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

-- filterEmptyValuesFromList : List String -> List String
-- filterEmptyValuesFromList  searchFilterList =
--     List.filter (
--                     \str -> 
--                         str
--                             |> String.trim
--                             |> String.isEmpty
--                             |> not 
--                 )
                
--                 searchFilterList

-- applyExtraOnSearchFilter  : Int -> List String -> Array String
-- applyExtraOnSearchFilter sortOrder searchFilterKeyValue =
--     filterDuplicates searchFilterKeyValue
--         |> filterEmptyValuesFromList
--         |> (if sortOrder == 0 then 
--                 List.sort 
--             else 
--                 List.sortWith desendingOrder)
--         |> Array.fromList

--buildSearchFilterValueRangeList : SearchFilterRangeUnionType -> Array SearchFilterRangeType ->  List Truck -> Array (String, Int)
buildSearchFilterValueRangeList : SearchFilterRangeUnionType -> Array SearchFilterRangeType ->  List Truck -> Array SearchFilterRangeType
buildSearchFilterValueRangeList searchFilterRangeUnionType searchFilterRangeTypes trucks =
    case searchFilterRangeUnionType of
        
        Price ->
             Array.indexedMap
                         (\index range -> 

                            SearchFilterRangeType   index 
                                                    range.searchFilterKey 
                                                    range.searchFilterMinValue  
                                                    range.searchFilterMaxValue False 
                                                    (List.length <| List.filter (\t -> t.price >= range.searchFilterMinValue && t.price <= range.searchFilterMaxValue) trucks) 
                                                    searchFilterRangeUnionType --using Constructor

                            --Tuple.pair range.searchFilterKey (List.length <| List.filter (\t -> t.price >= range.searchFilterMinValue && t.price <= range.searchFilterMaxValue) trucks)
                         )
                         
                        searchFilterRangeTypes
            
    

                --|> applyExtraOnSearchFilter 0
                -- |> Array.fromList
                -- |> (\sfArray -> 
                --                 Array.map (\sf -> 
                --                                 Tuple.pair (String.fromFloat sf) (List.length <| (List.filter (\t -> t.price == sf) trucks )) ) sfArray
                --     )

-- buildSearchFilterValuesRangeRecordList : SearchFilterRangeUnionType -> List Truck -> Array SearchFilterRangeType
-- buildSearchFilterValuesRangeRecordList searchFilterRangeUnionType trucks =
--     buildSearchFilterValueList searchFilterRangeUnionType trucks
--         |> Array.indexedMap 
--         (\index  sfValue -> 
--                         --{index = index, searchFilterKey = Tuple.first sfValue, userAction = False, resultCount = Tuple.second sfValue, filterCategory = searchFilterCustomType}
--                     SearchFilterRangeType index (Tuple.first sfValue) 100 200 False (Tuple.second sfValue) searchFilterRangeUnionType --using Constructor
--         )

buildSearchFilterValuesRangeGroup : SearchFilterRangeUnionType ->  Model -> UIModel -> Element Msg
buildSearchFilterValuesRangeGroup searchFilterRangeUnionType model uiModel =
    let
            (searchFilters, filterLabel, msg)
                =   case searchFilterRangeUnionType of
                            Price -> 
                                (uiModel.priceFilters, "Price", FilterRangeCheckBoxClicked)
            
            y = Debug.log "------------->" [searchFilters]

            searchFilterRangeState = 
                    uiModel.expandCollapseSearchFilterRangeStates
                            |> Array.filter (\mf -> mf.searchFilterRangeUnionType == searchFilterRangeUnionType)
                            |> Array.toList
                            |> List.head
                            |> (\possbileFirstItem ->
                                    case possbileFirstItem of
                                            Just val -> val
                                            Nothing -> SearchFilterRangeState -1 Price False -- Nothing case will never happen, but elm forces to handle all possibel cases
                                )

            buildCheckboxes :  Int -> SearchFilterRangeType -> Element Msg
            buildCheckboxes index searchFilter =
                let
                    x = Debug.log "------------->" [searchFilter]
                in
                
                    row[bw two, size 14]
                    [
                        checkbox [bw one, pdr 0 ] {
                            onChange = msg index searchFilterRangeUnionType
                            ,icon = buildChkBoxImage
                            , label = labelRight [centerY] (el [] <| textValue searchFilter.searchFilterKey )
                            , checked = searchFilter.userAction
                        }
                        , textValue <| " (" ++  (String.fromInt <| searchFilter.resultCount)  ++ ")"
                    ]
    in
        row[ wf, bw 0]
        [
            column[spy 0, wf,  bw one]
            [
                row[bw 0,  bwb 0, wf, pdb 1, bc 227 227 227]
                [
                    column[wf, hf][
                        paragraph [bw one, fal, wf, hpx 25, pd 5, centerY][textValue <| filterLabel]
                    ]
                    ,column[pdr 5][

                        -- checkbox [bw one,   far , bw 0] {
                        --             onChange = Nothing --CollapseClicked searchFilterState
                        --             ,icon = buildCollapseAllImage
                        --             , label = labelLeft [] <| none
                        --             , checked = 
                        --                     True
                        --                      --searchFilterState.userAction
                        --         }
                    ]
                ]
                ,column ( [spy 10, wf] ++ expandCollapseAll searchFilterRangeState.userAction)
                (
                    Array.toList <| Array.indexedMap buildCheckboxes searchFilters -- column function needs List of item and not Array of items, so need conversion
                )
            ]
        ]