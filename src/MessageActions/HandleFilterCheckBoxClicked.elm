module MessageActions.HandleFilterCheckBoxClicked exposing (..)

import Model exposing(..)
import BusinessFunctions.SearchFilterFunctions exposing(..)
import Array exposing (..)
import List.Extra exposing (..)
import BusinessFunctions.TruckFunctions exposing (sortTruckList)
import Helpers.Utils exposing (defaultTrucksPerPage)
import Commands exposing (fetchSearchFilterRanges)
import Task
import Process
import Msg exposing (..)
import Browser.Dom exposing (..)

handleFilterCheckBoxClicked selectedSearchFilter userAction model uiModel =

        let
                convertMaybeInt intValue =
                        case intValue of 
                                Just val -> 
                                        val
                                Nothing -> 
                                        0

                updateUserSelectedSearchFilter : Array SearchFilterType -> (Array SearchFilterType -> UIModel) -> UIModel -- Anonymous funcs
                updateUserSelectedSearchFilter  filterList pushModifiedFilterListBackInToUIModelFunc =
                        let
                                sfIndex =
                                        Array.toList filterList
                                                |> findIndex (\sf -> String.trim sf.searchFilterKey == String.trim selectedSearchFilter.searchFilterKey && sf.filterCategory == selectedSearchFilter.filterCategory)
                                                |> convertMaybeInt
                        in
                                if sfIndex > 0 then
                                        Array.toList filterList
                                                |> find (\sf -> String.trim sf.searchFilterKey == String.trim selectedSearchFilter.searchFilterKey  && sf.filterCategory == selectedSearchFilter.filterCategory)
                                                |> Maybe.map (\sf -> {sf | userAction = userAction})
                                                |> Maybe.map (\sf -> Array.set sfIndex sf filterList)
                                                |> Maybe.map pushModifiedFilterListBackInToUIModelFunc
                                                |> Maybe.withDefault uiModel
                                else
                                        uiModel

                newUIModel = 
                        partialSearchFiltersMetadata
                                |> find (\sfMeta -> sfMeta.filterName == selectedSearchFilter.filterCategory)
                                |> Maybe.map (
                                                \sfMeta -> ( 
                                                                (sfMeta.filters uiModel |> updateUserSelectedSearchFilter) (sfMeta.pushModifiedFilterListBackInToUIModel uiModel)
                                                )
                                                )
                                -- the below condition should never happen unless you misspell in metadata list in model.elm file
                                |> Maybe.withDefault uiModel

                newUIModelUpdatedWithSearchFilterBullets = 
                                {newUIModel |
                                                selectedFilterBullets = 
                                                        if userAction then
                                                                -- do not insert if the combo sf exists
                                                                newUIModel.selectedFilterBullets
                                                                        |> findIndex (\sf -> String.trim sf.searchFilterKey == String.trim selectedSearchFilter.searchFilterKey && sf.filterCategory == selectedSearchFilter.filterCategory)
                                                                        |> convertMaybeInt
                                                                        |> (\idx -> 
                                                                                let
                                                                                        selectedFilterBullet =  SearchFilterType -- TODO change this variable name
                                                                                                selectedSearchFilter.index 
                                                                                                selectedSearchFilter.searchFilterKey 
                                                                                                selectedSearchFilter.searchFilterExtraData
                                                                                                userAction
                                                                                                0
                                                                                                selectedSearchFilter.filterCategory 
                                                                                in
                                                                                
                                                                                        if idx > 0 then
                                                                                                newUIModel.selectedFilterBullets 
                                                                                        else
                                                                                                selectedFilterBullet :: newUIModel.selectedFilterBullets
                                                                                )
                                                        else
                                                                newUIModel.selectedFilterBullets
                                                                        |> find (\sf -> String.trim sf.searchFilterKey == String.trim selectedSearchFilter.searchFilterKey && sf.filterCategory == selectedSearchFilter.filterCategory)
                                                                        |> Maybe.map (\sf -> remove sf  newUIModel.selectedFilterBullets)
                                                                        |> Maybe.withDefault newUIModel.selectedFilterBullets
                                }

                newSortedFilteredTruckList = applySearchFilters model newUIModelUpdatedWithSearchFilterBullets
                                                |> sortTruckList uiModel.currentSortBy

                uiModelUpdatedWithLatestSearchFilters =
                        rebuildSearchFiltersBasedOnCurrentSearchCriteria model newUIModelUpdatedWithSearchFilterBullets
        in
                ( ( {model | filteredTruckList = newSortedFilteredTruckList, pagedTruckList = List.take defaultTrucksPerPage newSortedFilteredTruckList, currentPageNumber = 1 } , uiModelUpdatedWithLatestSearchFilters), Task.perform (\_ -> NOoP) (setViewport 0 0))
