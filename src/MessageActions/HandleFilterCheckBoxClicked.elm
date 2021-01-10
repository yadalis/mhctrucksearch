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
import Maybe

handleFilterCheckBoxClicked selectedSearchFilter userAction model uiModel =

        let
                vxx = Debug.log "OnFetchSearchFilterRanges " [selectedSearchFilter]
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
                                                        if userAction then -- user checked a filter
                                                                let
                                                                        selectedFilterBullet =  SearchFilterType -- TODO change this variable name
                                                                                                selectedSearchFilter.index 
                                                                                                selectedSearchFilter.searchFilterKey 
                                                                                                selectedSearchFilter.searchFilterExtraData
                                                                                                userAction
                                                                                                0
                                                                                                selectedSearchFilter.filterCategory 
                                                                in
                                                                        selectedFilterBullet :: newUIModel.selectedFilterBullets -- :: is called cons or appending to the end of the list
                                                                
                                                                -- do not insert if the combo sf exists
                                                                -- newUIModel.selectedFilterBullets
                                                                --         |> findIndex (\sf -> String.trim sf.searchFilterKey == String.trim selectedSearchFilter.searchFilterKey && sf.filterCategory == selectedSearchFilter.filterCategory)
                                                                --         --|> convertMaybeInt -- you can use Maybe.map to unwarp values from Maybe types
                                                                --         --so instead of writing a function convertMaybeInt to evalueate the index values returned by 
                                                                --         --findIndex function above, you can simply write Maybe.map and maybe.withDefautl
                                                                --         -- maybe.map takes an argument like idx in this case if findIndex actually returns a value
                                                                --         -- maybe.map withDefautl option handles when the index returned by findIndex is NOTHING
                                                                --         -- so with maybe.map and maybe.withDefault you can avoid writing functions to evalute
                                                                --         --maybe types
                                                                --         |> Maybe.map(\idx -> --if Index returned by findIndex func has a int value , the control comes here
                                                                --                 -- let
                                                                --                 --         selectedFilterBullet =  SearchFilterType -- TODO change this variable name
                                                                --                 --                 selectedSearchFilter.index 
                                                                --                 --                 selectedSearchFilter.searchFilterKey 
                                                                --                 --                 selectedSearchFilter.searchFilterExtraData
                                                                --                 --                 userAction
                                                                --                 --                 0
                                                                --                 --                 selectedSearchFilter.filterCategory 
                                                                --                 -- in
                                                                                
                                                                --                         --if idx > 0 then
                                                                --                                 newUIModel.selectedFilterBullets 
                                                                --                         --else
                                                                --                                 --selectedFilterBullet :: newUIModel.selectedFilterBullets
                                                                --                 )
                                                                --         |> Maybe.withDefault ( --if Index returned by findIndex func is nothing, then the control comes here
                                                                --                 let
                                                                --                         selectedFilterBullet =  SearchFilterType -- TODO change this variable name
                                                                --                                 selectedSearchFilter.index 
                                                                --                                 selectedSearchFilter.searchFilterKey 
                                                                --                                 selectedSearchFilter.searchFilterExtraData
                                                                --                                 userAction
                                                                --                                 0
                                                                --                                 selectedSearchFilter.filterCategory 
                                                                --                 in
                                                                --                                 selectedFilterBullet :: newUIModel.selectedFilterBullets -- :: is called cons or appending to the end of the list
                                                                --                 )
                                                        else -- user unchecked a filter
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
