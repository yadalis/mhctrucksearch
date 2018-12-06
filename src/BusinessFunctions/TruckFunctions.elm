module BusinessFunctions.TruckFunctions exposing (..)

import Element exposing (..)
import Element.Input exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Helpers.Utils exposing (..)
import Model exposing (..)
import Msg exposing (..)
import List.Unique exposing (..)
import Array exposing (..)
import SearchFilterViews.SearchFilter exposing (..)

rebuildSearchFiltersBasedOnCurrentSearchCriteria : Model -> UIModel -> UIModel
rebuildSearchFiltersBasedOnCurrentSearchCriteria model uiModel =
        let
                hasAnyOfSearchFilterValuesChecked searchFilters =
                        searchFilters
                                |> Array.filter (\sf -> sf.userAction == True) 
                                |> Array.length
                                |> (\length  -> length > 0)

                hasThisTruckMatchesWithUserSelectedFilterValue filterList partialCompareWaitingForSecondParamSearchFilter = 
                        filterList
                                |> Array.filter partialCompareWaitingForSecondParamSearchFilter
                                |> Array.length
                                |> (\length  -> length > 0)

                buildFilteredSearchResultBySearchType filterList comparefilterKeyValueWithTruckParam trucks =
                        if hasAnyOfSearchFilterValuesChecked filterList then
                                List.filter (\truck -> 
                                                hasThisTruckMatchesWithUserSelectedFilterValue filterList (comparefilterKeyValueWithTruckParam truck)
                                        )  trucks 
                        else
                                trucks

                findMatchAndSetUserAction filters sf =
                        filters
                                |> Array.filter(\uiSF -> uiSF.searchFilterKey == sf.searchFilterKey)
                                |> Array.toList
                                |> List.head
                                |> (\headItem -> 
                                        case headItem of 
                                                Just val -> val
                                                Nothing -> sf)
                                |> (\headItem -> {sf | userAction = headItem.userAction} )

                updatedSalesStatusFitlerList =
                        model.truckList
                                |> (buildFilteredSearchResultBySearchType uiModel.yearFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.year && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.makeFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.make && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.modelFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.model && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperRoofFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperRoof && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperBunkFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperBunk && sf.userAction == True )
                                >> buildSearchFilterValueRecordList SalesStatus
                                        >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.salesStatusFilters sf 
                                        )

                --z = Debug.log "current sales list"  [uiModel.salesStatusFilters]--, newUIModel1.yearFilters]

          
                

                -- newUpdatedSalesStatusFitlerList =
                --         updatedSalesStatusFitlerList
                --                 |> Array.map(\sf ->
                --                                        findMatchAndSetUserAction uiModel.salesStatusFilters sf 
                --                         )


                --g = Debug.log "updated sales list"  [newUpdatedSalesStatusFitlerList]--, newUIModel1.yearFilters]
                                           
                        
                
                -- lst =
                --         model.truckList
                --                 |> (buildFilteredSearchResultBySearchType uiModel.salesStatusFilters)
                --                         (\t sf -> String.trim sf.searchFilterKey == String.trim t.salesStatus && sf.userAction == True )        
                
                --y = Debug.log "Updated list by held sales status"  [lst]--, newUIModel1.yearFilters]
                        
                updatedYearFitlerList =
                        model.truckList
                                |> (buildFilteredSearchResultBySearchType uiModel.salesStatusFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.salesStatus && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.makeFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.make && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.modelFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.model && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperRoofFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperRoof && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperBunkFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperBunk && sf.userAction == True )
                                >> buildSearchFilterValueRecordList Year
                                        >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.yearFilters sf 
                                        )
                
                -- newUpdatedYearFitlerList =
                --         updatedYearFitlerList
                --                 |> Array.map(\sf ->
                --                                        findMatchAndSetUserAction uiModel.yearFilters sf 
                --                         )

                updatedMakeFitlerList =
                        model.truckList
                                |> (buildFilteredSearchResultBySearchType uiModel.salesStatusFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.salesStatus && sf.userAction == True )
                                |> (buildFilteredSearchResultBySearchType uiModel.yearFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.year && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.modelFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.model && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperRoofFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperRoof && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperBunkFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperBunk && sf.userAction == True )
                                >> buildSearchFilterValueRecordList Make
                                        >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.makeFilters sf 
                                        )
                
                updatedModelFitlerList =
                        model.truckList
                                |> (buildFilteredSearchResultBySearchType uiModel.salesStatusFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.salesStatus && sf.userAction == True )
                                |> (buildFilteredSearchResultBySearchType uiModel.yearFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.year && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.makeFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.model && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperRoofFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperRoof && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperBunkFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperBunk && sf.userAction == True )
                                >> buildSearchFilterValueRecordList MakeModel
                                        >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.modelFilters sf 
                                        )

                updatedSleeperRoofFitlerList =
                        model.truckList
                                |> (buildFilteredSearchResultBySearchType uiModel.salesStatusFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.salesStatus && sf.userAction == True )
                                |> (buildFilteredSearchResultBySearchType uiModel.yearFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.year && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.makeFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.model && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.modelFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperRoof && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperBunkFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperBunk && sf.userAction == True )
                                >> buildSearchFilterValueRecordList SleeperRoof
                                        >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.sleeperRoofFilters sf 
                                        )

                updatedSleeperBunkFitlerList =
                        model.truckList
                                |> (buildFilteredSearchResultBySearchType uiModel.salesStatusFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.salesStatus && sf.userAction == True )
                                |> (buildFilteredSearchResultBySearchType uiModel.yearFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.year && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.makeFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.model && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.modelFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperRoof && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperRoofFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperBunk && sf.userAction == True )
                                >> buildSearchFilterValueRecordList SleeperBunk
                                        >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.sleeperBunkFilters sf 
                                        )
                

                -- newupdatedMakeFitlerList =
                --         updatedMakeFitlerList
                --                 |> Array.map(\sf ->
                --                                        findMatchAndSetUserAction uiModel.makeFilters sf 
                --                         )
                -- y = Debug.log "Updated salesstatus list by year"  [updatedSalesStatusFitlerList]--, newUIModel1.yearFilters]
                -- z = Debug.log "---------------------------------------------------------------------------------------------------"  [] --, newUIModel1.yearFilters]
                -- c = Debug.log "Updated year list by held salesstatus"  [updatedYearFitlerList]--, newUIModel1.yearFilters]
                -- e = Debug.log "---------------------------------------------------------------------------------------------------"  [] --, newUIModel1.yearFilters]
                -- a = Debug.log "Updated make list by held salesstatus"  [updatedMakeFitlerList]--, newUIModel1.yearFilters]
                        --         salesStatusFilters = buildSearchFilterValueRecordList SalesStatus trucks
                        -- yearFilters = buildSearchFilterValueRecordList Year trucks
                        -- makeFilters = buildSearchFilterValueRecordList Make trucks
                        -- modelFilters = buildSearchFilterValueRecordList MakeModel trucks
                        -- sleeperRoofFilters = buildSearchFilterValueRecordList SleeperRoof trucks
                        -- sleeperBunkFilters = buildSearchFilterValueRecordList SleeperBunk trucks



                newUIModel = 
                        {
                                uiModel |
                                                yearFilters = updatedYearFitlerList
                                                , salesStatusFilters = updatedSalesStatusFitlerList
                                                , makeFilters = updatedMakeFitlerList
                                                , modelFilters = updatedModelFitlerList
                                                , sleeperRoofFilters = updatedSleeperRoofFitlerList
                                                , sleeperBunkFilters = updatedSleeperBunkFitlerList
                        }
        in
                newUIModel

applySearchFilters: Model -> UIModel -> List Truck
applySearchFilters model uiModel =
    let
        hasAnyOfSearchFilterValuesChecked searchFilters =
            searchFilters
                |> Array.filter (\sf -> sf.userAction == True) 
                |> Array.length
                |> (\length  -> length > 0)

        hasThisTruckMatchesWithUserSelectedFilterValue filterList partialCompareWaitingForSecondParamSearchFilter = 
            filterList
                |> Array.filter partialCompareWaitingForSecondParamSearchFilter
                |> Array.length
                |> (\length  -> length > 0)

        buildFilteredSearchResultBySearchType filterList comparefilterKeyValueWithTruckParam trucks =
             if hasAnyOfSearchFilterValuesChecked filterList then
                List.filter (\truck -> 
                                    hasThisTruckMatchesWithUserSelectedFilterValue filterList (comparefilterKeyValueWithTruckParam truck)
                            )  trucks 
            else
                trucks

        filterdTruckList  = 
                model.truckList
                        |> (buildFilteredSearchResultBySearchType uiModel.salesStatusFilters)
                                (\t sf -> String.trim sf.searchFilterKey == String.trim t.salesStatus && sf.userAction == True ) -- truckList gets passed as a last arg automatically from the previous |> pipe
                                -- the result from the above function gets feed in to the below function and so on until it ends
                        >> (buildFilteredSearchResultBySearchType uiModel.yearFilters)
                                (\t sf -> String.trim sf.searchFilterKey == String.trim t.year && sf.userAction == True )
                        >> (buildFilteredSearchResultBySearchType uiModel.makeFilters)
                                (\t sf -> String.trim sf.searchFilterKey == String.trim t.make && sf.userAction == True )
                        >> (buildFilteredSearchResultBySearchType uiModel.modelFilters)
                                (\t sf -> String.trim sf.searchFilterKey == String.trim t.model && sf.userAction == True )
                        >> (buildFilteredSearchResultBySearchType uiModel.sleeperRoofFilters)
                                (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperRoof && sf.userAction == True )
                        >> (buildFilteredSearchResultBySearchType uiModel.sleeperBunkFilters)
                                (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperBunk && sf.userAction == True )

        sortedFilterdTruckList =
            filterdTruckList
                |> List.sortBy .make
    in
        sortedFilterdTruckList
