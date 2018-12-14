
-- module SearchFilterViews.SearchFilterRage exposing (..)

-- import Element exposing (..)
-- import Element.Input exposing (..)
-- import Element.Font exposing (..)
-- import Helpers.ElmStyleShotcuts exposing (..)
-- import Helpers.ElmUI exposing (..)
-- import Helpers.Utils exposing (..)
-- import Model exposing (..)
-- import Msg exposing (..)
-- import List.Unique exposing (..)
-- import Array exposing (..)


-- getMinMaxValue range =
--         let
            
--                 minmaxValues = String.split "-" range.searchFilterExtraData -- "a:THERMOKING", "md:t880", "y:2019" etc...
--                 minValue =     
--                         case List.head <| minmaxValues of -- gives first element in the list
--                         Just strMinVal -> case String.toInt strMinVal of 
--                                                 Just minVal -> minVal
--                                                 Nothing -> 0                    
--                         Nothing -> 0
--                 maxValue =
--                         -- case List.foldl (Just >> always) Nothing searchFilterValueList of  -- gives last element in the list -- 1st style
--                         --     Just val -> val
--                         --     Nothing -> ""
--                         case List.head <| List.reverse minmaxValues of -- gives last element in the list -- 2nd style
--                                 Just strMaxVal -> case String.toInt strMaxVal of 
--                                                 Just maxVal -> maxVal
--                                                 Nothing -> 0     
--                                 Nothing -> 0     
--         in
--                 (minValue, maxValue)
 
-- --flippedComparison a b =
-- desendingOrder a b =
--     case compare a b of
--         LT -> GT
--         EQ -> EQ
--         GT -> LT

-- -- filterEmptyValuesFromList : List String -> List String
-- -- filterEmptyValuesFromList  searchFilterList =
-- --     List.filter (
-- --                     \str -> 
-- --                         str
-- --                             |> String.trim
-- --                             |> String.isEmpty
-- --                             |> not 
-- --                 )
                
-- --                 searchFilterList

-- -- applyExtraOnSearchFilter  : Int -> List String -> Array String
-- -- applyExtraOnSearchFilter sortOrder searchFilterKeyValue =
-- --     filterDuplicates searchFilterKeyValue
-- --         |> filterEmptyValuesFromList
-- --         |> (if sortOrder == 0 then 
-- --                 List.sort 
-- --             else 
-- --                 List.sortWith desendingOrder)
-- --         |> Array.fromList

-- --buildSearchFilterValueRangeList : SearchFilterRangeUnionType -> Array SearchFilterRangeType ->  List Truck -> Array (String, Int)
-- buildSearchFilterValueRangeList : SearchFilterCustomType -> Array SearchFilterType ->  List Truck -> Array SearchFilterType
-- buildSearchFilterValueRangeList searchFilterCustomType searchFilterTypes trucks =
--     case searchFilterCustomType of

--         Price ->
--              Array.indexedMap
--                          (\index range -> 

--                             let
                                
--                                 minmaxValue = getMinMaxValue range    
                                
--                                 minValue = Tuple.first minmaxValue
--                                 maxValue = Tuple.second minmaxValue

--                             in
                            
--                             SearchFilterType   index 
--                                                     range.searchFilterKey 
--                                                     range.searchFilterExtraData 
--                                                     -- range.searchFilterMinValue  
--                                                     -- range.searchFilterMaxValue 
--                                                     False 
--                                                     (List.length <| List.filter (\t -> t.price >= minValue && t.price <= maxValue) trucks) 
--                                                     searchFilterCustomType --using Constructor

--                             --Tuple.pair range.searchFilterKey (List.length <| List.filter (\t -> t.price >= range.searchFilterMinValue && t.price <= range.searchFilterMaxValue) trucks)
--                          )
                         
--                         searchFilterTypes
            
            
--         _ ->
--             Array.empty
            

--                 --|> applyExtraOnSearchFilter 0
--                 -- |> Array.fromList
--                 -- |> (\sfArray -> 
--                 --                 Array.map (\sf -> 
--                 --                                 Tuple.pair (String.fromFloat sf) (List.length <| (List.filter (\t -> t.price == sf) trucks )) ) sfArray
--                 --     )

-- -- buildSearchFilterValuesRangeRecordList : SearchFilterRangeUnionType -> List Truck -> Array SearchFilterRangeType
-- -- buildSearchFilterValuesRangeRecordList searchFilterRangeUnionType trucks =
-- --     buildSearchFilterValueList searchFilterRangeUnionType trucks
-- --         |> Array.indexedMap 
-- --         (\index  sfValue -> 
-- --                         --{index = index, searchFilterKey = Tuple.first sfValue, userAction = False, resultCount = Tuple.second sfValue, filterCategory = searchFilterCustomType}
-- --                     SearchFilterRangeType index (Tuple.first sfValue) 100 200 False (Tuple.second sfValue) searchFilterRangeUnionType --using Constructor
-- --         )

-- buildSearchFilterValuesRangeGroup : SearchFilterCustomType ->  Model -> UIModel -> Element Msg
-- buildSearchFilterValuesRangeGroup searchFilterCustomType model uiModel =
--     let
--             (searchFilters, filterLabel, msg)
--                 =   case searchFilterCustomType of
--                             Price -> 
--                                 (uiModel.priceFilters, "Price", FilterCheckBoxClicked)
--                             _ -> 
--                                 (uiModel.priceFilters, "Price", FilterCheckBoxClicked)
            
--             y = Debug.log "------------->" [searchFilters]

--             searchFilterRangeState = 
--                     uiModel.expandCollapseSearchFilterStates
--                             |> Array.filter (\mf -> mf.searchFilterCustomType == searchFilterCustomType)
--                             |> Array.toList
--                             |> List.head
--                             |> (\possbileFirstItem ->
--                                     case possbileFirstItem of
--                                             Just val -> val
--                                             Nothing -> SearchFilterState -1 Price False -- Nothing case will never happen, but elm forces to handle all possibel cases
--                                 )

--             buildCheckboxes :  Int -> SearchFilterType -> Element Msg
--             buildCheckboxes index searchFilter =
--                 let
--                     x = Debug.log "------------->" [searchFilter]
--                 in
                
--                     if searchFilter.resultCount > 0 then 
--                         row[bw two, size 14]
--                         [
--                             checkbox [bw one, pdr 0 ] {
--                                 onChange = msg index searchFilterCustomType
--                                 ,icon = buildChkBoxImage
--                                 , label = labelRight [centerY] (el [] <| textValue searchFilter.searchFilterKey )
--                                 , checked = searchFilter.userAction
--                             }
--                             , textValue <| " (" ++  (String.fromInt <| searchFilter.resultCount)  ++ ")"
--                         ]
--                     else
--                         none
--     in
--         row[ wf, bw 0]
--         [
--             column[spy 0, wf,  bw one]
--             [
--                 row[bw 0,  bwb 0, wf, pdb 1, bc 227 227 227]
--                 [
--                     column[wf, hf][
--                         paragraph [bw one, fal, wf, hpx 25, pd 5, centerY][textValue <| filterLabel]
--                     ]
--                     ,column[pdr 5][

--                         checkbox [bw one,   far , bw 0] {
--                                     onChange = CollapseClicked searchFilterRangeState
--                                     ,icon = buildCollapseAllImage
--                                     , label = labelLeft [] <| none
--                                     , checked = searchFilterRangeState.userAction
--                                 }
--                     ]
--                 ]
--                 ,column ( [spy 10, wf] ++ expandCollapseAll searchFilterRangeState.userAction)
--                 (
--                     Array.toList <| Array.indexedMap buildCheckboxes searchFilters -- column function needs List of item and not Array of items, so need conversion
--                 )
--             ]
--         ]