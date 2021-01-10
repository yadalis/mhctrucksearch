module MessageActions.HandleClearAllFilters exposing (..)
import BusinessFunctions.SearchFilterFunctions exposing (resetFilters)
import BusinessFunctions.SearchFilterFunctions exposing (partialSearchFiltersMetadata)
import BusinessFunctions.TruckFunctions exposing (sortTruckList)
import Helpers.Utils exposing (defaultTrucksPerPage)
import BusinessFunctions.SearchFilterFunctions exposing (rebuildSearchFiltersBasedOnCurrentSearchCriteria)


handleClearAllFilters model uiModel =
    let
        executeRegularAndRangeFilterFunc filterMeta updatableUIModel =
            filterMeta.pushModifiedFilterListBackInToUIModel 
                updatableUIModel 
                (resetFilters (filterMeta.filters updatableUIModel))

        newUIModelx = 
            List.foldl
                    executeRegularAndRangeFilterFunc
                    uiModel
                    partialSearchFiltersMetadata

        newUIModel = {newUIModelx | selectedFilterBullets = []}
        
        sortedTrkList = model.truckList |> sortTruckList uiModel.currentSortBy

        newModel = 
            {model |
                    filteredTruckList = sortedTrkList,
                    pagedTruckList =List.take defaultTrucksPerPage sortedTrkList
                    ,currentPageNumber = 1}

        uiModelUpdatedWithLatestSearchFilters =
                rebuildSearchFiltersBasedOnCurrentSearchCriteria newModel newUIModel
    in
        ( (newModel, uiModelUpdatedWithLatestSearchFilters), Cmd.none )

