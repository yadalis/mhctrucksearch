
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
 

getMinMaxValue range =
        let
            
                minmaxValues = String.split "-" range.searchFilterExtraData -- "a:THERMOKING", "md:t880", "y:2019" etc...
                minValue =     
                        case List.head <| minmaxValues of -- gives first element in the list
                        Just strMinVal -> case String.toFloat strMinVal of 
                                                Just minVal -> minVal
                                                Nothing -> 0                    
                        Nothing -> 0
                maxValue =
                        -- case List.foldl (Just >> always) Nothing searchFilterValueList of  -- gives last element in the list -- 1st style
                        --     Just val -> val
                        --     Nothing -> ""
                        case List.head <| List.reverse minmaxValues of -- gives last element in the list -- 2nd style
                                Just strMaxVal -> case String.toFloat strMaxVal of 
                                                Just maxVal -> maxVal
                                                Nothing -> 0     
                                Nothing -> 0     
        in
                (minValue, maxValue)


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

--buildSearchFilterValueList : SearchFilterCustomType ->  Array SearchFilterType -> List Truck -> Array (String, Int)
buildSearchFilterValueList : SearchFilterCustomType ->  Array SearchFilterType -> List Truck -> Array SearchFilterType
buildSearchFilterValueList searchFilterCustomType searchFilterTypes trucks =
    case searchFilterCustomType of
        SalesStatus -> 
            --List.map (\t -> t.salesStatus) trucks
            List.map .salesStatus trucks
                |> applyExtraOnSearchFilter 0
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                               -- Tuple.pair sf (List.length <| (List.filter (\t -> String.trim t.salesStatus == sf) trucks )) ) sfArray
                                               SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.salesStatus == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                                    --SearchFilterType index (Tuple.first sfValue) (Tuple.first sfValue) False (Tuple.second sfValue) searchFilterCustomType --using Constructor
                    ) 

        Year -> 
            --List.map (\t -> t.year) trucks
            List.map .year trucks
                |> applyExtraOnSearchFilter 1
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.year == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                                                --Tuple.pair sf (List.length <| (List.filter (\t -> String.trim t.year == sf) trucks )) ) sfArray
                    )
                
        Make -> 
            --List.map (\t -> t.make) trucks
            List.map .make trucks
                |> applyExtraOnSearchFilter 0
                |> (\sfArray -> 
                                Array.indexedMap (\index sf ->  
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.make == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                

        MakeModel -> 
            --List.map (\t -> t.model) trucks
            List.map .model trucks
                |> applyExtraOnSearchFilter 0
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.model == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                

        SleeperRoof -> 
            --List.map (\t -> t.sleeperRoof) trucks
            List.map .sleeperRoof trucks
                |> applyExtraOnSearchFilter 0
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.sleeperRoof == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                
                
        SleeperBunk -> 
            --List.map (\t -> t.sleeperBunk) trucks
            List.map .sleeperBunk trucks
                |> applyExtraOnSearchFilter 0
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.sleeperBunk == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )

        Price ->
             Array.indexedMap
                         (\index range -> 

                            let
                                minmaxValue = getMinMaxValue range     
                                minValue = Tuple.first minmaxValue
                                maxValue = Tuple.second minmaxValue
                            in
                                --Tuple.pair range.searchFilterExtraData ( (List.length <| List.filter (\t -> t.price >= minValue && t.price <= maxValue) trucks)  )
                                --using Constructor style
                                SearchFilterType   index 
                                                        range.searchFilterKey 
                                                        range.searchFilterExtraData 
                                                        -- range.searchFilterMinValue  
                                                        -- range.searchFilterMaxValue 
                                                        False 
                                                        (List.length <| List.filter (\t -> t.price >= minValue && t.price <= maxValue) trucks) 
                                                        searchFilterCustomType 

                         )
                         
                        searchFilterTypes

        EngineHP ->
             Array.indexedMap
                         (\index range -> 

                            let
                                minmaxValue = getMinMaxValue range     
                                minValue = Tuple.first minmaxValue
                                maxValue = Tuple.second minmaxValue
                            in
                                --Tuple.pair range.searchFilterExtraData ( (List.length <| List.filter (\t -> t.price >= minValue && t.price <= maxValue) trucks)  )
                                --using Constructor style
                                SearchFilterType   index 
                                                        range.searchFilterKey 
                                                        range.searchFilterExtraData 
                                                        -- range.searchFilterMinValue  
                                                        -- range.searchFilterMaxValue 
                                                        False 
                                                        (List.length <| List.filter (\t -> t.engineHP >= minValue && t.engineHP <= maxValue) trucks) 
                                                        searchFilterCustomType 

                         )
                         
                        searchFilterTypes

buildSearchFilterValueRecordList : SearchFilterCustomType -> Array SearchFilterType -> List Truck -> Array SearchFilterType
buildSearchFilterValueRecordList searchFilterCustomType searchFilterTypes trucks =
    buildSearchFilterValueList searchFilterCustomType searchFilterTypes trucks
        -- |> Array.indexedMap 
        --     (\index  sfValue -> 
        --                     --{index = index, searchFilterKey = Tuple.first sfValue, userAction = False, resultCount = Tuple.second sfValue, filterCategory = searchFilterCustomType}
        --                 SearchFilterType index (Tuple.first sfValue) (Tuple.first sfValue) False (Tuple.second sfValue) searchFilterCustomType --using Constructor
        --     )

buildSearchFilterValuesGroup : SearchFilterCustomType ->  Model -> UIModel -> Element Msg
buildSearchFilterValuesGroup searchFilterCustomType model uiModel =
    let
            (searchFilters, filterLabel, msg)
                =   case searchFilterCustomType of
                            SalesStatus -> 
                                (uiModel.salesStatusFilters, "Sales Status", FilterCheckBoxClicked)

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
                            
                            Price -> 
                                (uiModel.priceFilters, "Price", FilterCheckBoxClicked)
                            
                            EngineHP -> 
                                (uiModel.engineHPFilters, "HP", FilterCheckBoxClicked)

            searchFilterState = 
                    uiModel.expandCollapseSearchFilterStates
                            |> Array.filter (\mf -> mf.searchFilterCustomType == searchFilterCustomType)
                            |> Array.toList
                            |> List.head
                            |> (\possbileFirstItem ->
                                    case possbileFirstItem of
                                            Just val -> val
                                            Nothing -> SearchFilterState -1 SalesStatus False -- Nothing case will never happen, but elm forces to handle all possibel cases
                                )

            buildCheckboxes :  Int -> SearchFilterType -> Element Msg
            buildCheckboxes index searchFilter =
                if searchFilter.resultCount > 0 then
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
                else
                    none
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
                        checkbox [bw one,   far , bw 0] {
                                    onChange = CollapseClicked searchFilterState
                                    ,icon = buildCollapseAllImage
                                    , label = labelLeft [] <| none
                                    , checked =
                                             searchFilterState.userAction
                                }
                    ]
                ]
                ,column ( [spy 10, wf] ++ expandCollapseAll searchFilterState.userAction)
                (
                    Array.toList <| Array.indexedMap buildCheckboxes searchFilters -- column function needs List of item and not Array of items, so need conversion
                )
            ]
        ]