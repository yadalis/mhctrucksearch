module MessageActions.HandleOnFetchTrucks exposing (..)

import Model exposing(..)
import BusinessFunctions.SearchFilterFunctions exposing(..)
import Array exposing (..)
import List.Extra exposing (..)
import BusinessFunctions.TruckFunctions exposing (sortTruckList)
import Helpers.Utils exposing (defaultTrucksPerPage)
import Commands exposing (fetchSearchFilterRanges)

handleOnFetchTrucks response model uiModel =

    let
        vxx = Debug.log "OnFetchSearchFilterRanges " [response]

        trucks = 
                case response of
                        Ok truckList ->
                                truckList
                                        |> sortTruckList uiModel.currentSortBy
                        Err err ->
                                []
                        
                        --vx = Debug.log "asdfasfasd" [trucks]

        executeRegularFilterFunc regularFilterMeta currentUIModel =
                regularFilterMeta.pushModifiedFilterListBackInToUIModel 
                                        currentUIModel 
                                        (buildSearchFilterValueRecordList SingleValue regularFilterMeta.filterName (regularFilterMeta.filters currentUIModel)  trucks)

        (newModel, newUIModel, cmd) = 
                if List.length trucks > 0 then
                        (
                                {model  | truckList = trucks,  filteredTruckList = trucks, pagedTruckList = List.take defaultTrucksPerPage trucks },
                                List.foldl
                                        executeRegularFilterFunc
                                        uiModel
                                        (List.filter (\fltrMeta -> fltrMeta.filterStyle == SingleValue) partialSearchFiltersMetadata),
                                fetchSearchFilterRanges
                        )
                else
                        (
                                model,  
                                uiModel,
                                Cmd.none
                        )

        updatedUIModel =
                if List.length trucks > 0 then
                        {newUIModel | hasWarningsToPresent = False, userWarningMessage = "" }
                else
                        {newUIModel | hasWarningsToPresent = True, userWarningMessage = "No trucks found for the text " ++ uiModel.searchString }

    in
         ( ( newModel , updatedUIModel), cmd)