module MessageActions.HandleOnFetchSearchFilterRanges exposing (..)

import Model exposing(..)
import BusinessFunctions.SearchFilterFunctions exposing(..)
import Array exposing (..)
import List.Extra exposing (..)

handleOnFetchSearchFilterRanges response model uiModel =

    let
        vxx = Debug.log "OnFetchSearchFilterRanges " [response]

        rangeSearchFilters = 
                    case response of
                            Ok rangeFltrs ->
                                    rangeFltrs

                            Err err ->
                                    []

        allRangeSearchFiltersWithNoCountsWithItsFilterType = 
                                    List.map 
                                            (
                                                \eachRangeFilterType ->  
                                                            (eachRangeFilterType, List.filter (\sf -> sf.filterCategory == eachRangeFilterType ) rangeSearchFilters)
                                            ) 
                                    --allRangeFilterTypesMasterList -- make sure this list is up to date with all possible range filters, this is defined in model.elm
                                    (List.map (\rangeFltrMeta -> rangeFltrMeta.filterName) rangeSearchFiltersInitialExpandState)
        
        allRangeSearchFiltersWithCountsWithItsFilterType = 
                    List.map 
                            (
                                \(rangeFltrType, rangeFltrWithNoCountsList) ->  
                                        (rangeFltrType, (buildSearchFilterValueRecordList RangeValue rangeFltrType (Array.fromList <| rangeFltrWithNoCountsList) model.truckList ))
                            ) 
                    allRangeSearchFiltersWithNoCountsWithItsFilterType

        
        fetchRangeFiltersPoplulatedWithCounts fltrType = 
                    case    (find (\(rangeFltrType,filtrArray)  -> rangeFltrType == fltrType ) allRangeSearchFiltersWithCountsWithItsFilterType) of
                        Just item -> Tuple.second item
                        Nothing -> Array.empty

        executeRangeFilterFunc rangeFilterMeta currentUIModel =
            rangeFilterMeta.pushModifiedFilterListBackInToUIModel currentUIModel (fetchRangeFiltersPoplulatedWithCounts rangeFilterMeta.filterName)

        newUIModel = 
            List.foldl
                    executeRangeFilterFunc
                    uiModel
                    (List.filter (\fltrMeta -> fltrMeta.filterStyle == RangeValue) partialSearchFiltersMetadata)

    in
        --( ( model , {newUIModel | selectedFilterBullets = [] }), Cmd.none)--sendMessage ( FilterCheckBoxClicked 0 SalesStatus True ) )
        ( ( model , {newUIModel | selectedFilterBullets = [] }), Cmd.none)