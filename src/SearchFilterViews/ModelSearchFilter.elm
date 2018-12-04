
module SearchFilterViews.ModelSearchFilter exposing (..)

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
        
buildModelValueList : List Truck -> Array String
buildModelValueList trucks =
    List.map (\t -> t.model) trucks
        |> filterDuplicates
        |> List.sort -- to do descending order
        |> Array.fromList

buildModelValueRecordList : List Truck -> Array SearchFilterType
buildModelValueRecordList trucks =
    buildModelValueList trucks
        |> Array.map (\model -> {searchFilterKey = model, userAction = False, resultCount = 0})

buildModelValuesGroup : Model -> UIModel -> Element Msg
buildModelValuesGroup model uiModel = --currentFilteredTrucks =
    let
        modelFilters = uiModel.modelFilters

        buildModelCheckboxes :  Int -> SearchFilterType -> Element Msg
        buildModelCheckboxes index modelSearchFilter =
            let
                modelWiseCount =    List.filter (\t -> String.trim t.model == modelSearchFilter.searchFilterKey) model.filteredTruckList --currentFilteredTrucks
            in
                row[bw two]
                [
                    checkbox [bw one, pdr 5 ] {
                        onChange = FilterModelCheckBoxClicked index
                        ,icon = buildChkBoxImage
                        , label = labelRight [] (el [] <| textValue modelSearchFilter.searchFilterKey )
                        --, checked = uiModel.filterSelectionsModel.filterCDLNoSelected
                        , checked = modelSearchFilter.userAction
                    }
                    , textValue <| " (" ++  (String.fromInt <| (List.length modelWiseCount))  ++ ")"
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
                    paragraph [bw one, fal, wf, bc 200 200 200, hpx 25, pd 5, centerY][textValue <| "Model"]
                ]
                ,column[spy 10, pdl 15, hf, scrollbarY, wf]
                (
                    Array.toList <| Array.indexedMap buildModelCheckboxes modelFilters -- column function needs List of item and not Array of items, so need conversion
                )
            ]
        ]

buildChkBoxImage userAction =
        if userAction == True then 
            image [hpx 24] {src = "checked.png", description ="Logo" }
        else 
            el [hpx 24, wpx 24, bw 2, br 5] <| none