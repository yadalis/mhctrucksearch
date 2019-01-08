module SearchFilterViews.SearchFilter exposing (..)

import Element exposing (..)
import Element.Input exposing (..)
import Element.Font exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Model exposing (..)
import Msg exposing (..)
import List.Unique exposing (..)
import Array exposing (..)
import Helpers.Colors exposing (..)
import BusinessFunctions.SearchFilterFunctions exposing (..)
import List.Extra exposing (..)

buildSearchFilterValuesGroup : SearchFilterCustomType ->  Model -> UIModel -> Element Msg
buildSearchFilterValuesGroup searchFilterCustomType model uiModel =
    let
            (searchFilters, filterLabel, msg) = 
                partialSearchFiltersMetadata
                    |> find (\sfMeta -> sfMeta.filterName == searchFilterCustomType)
                    |> Maybe.map (\sfMeta -> (sfMeta.filters <| uiModel, sfMeta.displayText, FilterCheckBoxClicked))
                    -- the below condition should never happen unless you misspell in metadata list in model.elm file
                    |> Maybe.withDefault (defaultSearchFiltersMetadata.filters, defaultSearchFiltersMetadata.displayText, FilterCheckBoxClicked)

            showLabelRed = searchFilters
                                |> Array.toList
                                |> List.any (\sf -> sf.userAction)
                                |> (\isAnyFilterChecked -> 
                                            if isAnyFilterChecked then
                                                mhcRed
                                            else
                                                greyFont 0
                                    )
            
            searchFilterState = 
                    uiModel.expandCollapseSearchFilterStates
                            |> Array.filter (\mf -> mf.searchFilterCustomType == searchFilterCustomType)
                            |> Array.toList
                            |> List.head
                            |> (\possbileFirstItem ->
                                    case possbileFirstItem of
                                            Just val -> val
                                            Nothing -> SearchFilterState -1 FleetCode False -- Nothing case will never happen, but elm forces to handle all possibel cases
                                )

            --buildCheckbox :  Int -> SearchFilterType -> Element Msg
            buildCheckbox :  SearchFilterType -> Element Msg
            buildCheckbox searchFilter =
                let
                    chkBoxStyle =
                                    if searchFilter.userAction then 
                                        [mhcRed, fb ]
                                    else
                                        [greyFont 0]
                    -- displayValue = 
                    --                 -- if searchFilter.filterCategory == TruckType then
                    --                 --     searchFilter.searchFilterExtraData
                    --                 -- else
                    --                     searchFilter.searchFilterKey
                    
                    --updatedSearchFilter = {searchFilter | index = index, filterCategory = searchFilterCustomType }
                in
                    if searchFilter.resultCount > 0 then
                        row[wf, size 14, pdl 25]
                        [
                            checkbox [bwb 1, wf, pdb 5, greyBorder 175 ] {
                                --onChange = msg index searchFilterCustomType searchFilter.searchFilterKey searchFilter.searchFilterExtraData --(String.trim displayValue)
                                onChange = msg searchFilter --updatedSearchFilter
                                ,icon = buildChkBoxImage
                                , label = labelRight ([centerY] ++ chkBoxStyle)  (el [] <| textValue (searchFilter.searchFilterKey ++ " (" ++  (String.fromInt <| searchFilter.resultCount)  ++ ")"))
                                , checked = searchFilter.userAction
                            }
                        ]
                    else
                        none
    in
        if Array.length searchFilters > 0 then
            row[ wf, pdt 5]
            [
                column[wf]
                [
                    checkbox [] 
                    {
                        onChange = CollapseClicked searchFilterState
                        ,icon = buildCollapseAllImage
                        , label = labelRight [fs 14, showLabelRed] <|  textValue <| filterLabel
                        , checked =
                                    searchFilterState.userAction
                    }
                    ,column ( [spy 8, wf] ++ expandCollapseAll searchFilterState.userAction)
                    (
                        --Array.toList <| Array.indexedMap buildCheckbox searchFilters -- column function needs List of item and not Array of items, so need conversion
                        Array.toList <| Array.map buildCheckbox searchFilters -- column function needs List of item and not Array of items, so need conversion
                    )
                ]
            ]
        else
            none