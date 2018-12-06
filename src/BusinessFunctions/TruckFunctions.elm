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

        buildFilteredSearchResultBySearchType filterList comparefilterKeyValueWithTruck trucks =
             if hasAnyOfSearchFilterValuesChecked filterList then
                List.filter (\truck -> 
                                    hasThisTruckMatchesWithUserSelectedFilterValue filterList (comparefilterKeyValueWithTruck truck)
                            )  trucks 
            else
                trucks

        filterdTruckList  = 
            model.truckList
                |> (buildFilteredSearchResultBySearchType uiModel.salesStatusFilters)
                                 (\t sf -> String.trim sf.searchFilterKey == String.trim t.salesStatus && sf.userAction == True )
                >> (buildFilteredSearchResultBySearchType uiModel.yearFilters)
                                 (\t sf -> String.trim sf.searchFilterKey == String.trim t.year && sf.userAction == True )
                |> (buildFilteredSearchResultBySearchType uiModel.makeFilters)
                                 (\t sf -> String.trim sf.searchFilterKey == String.trim t.make && sf.userAction == True )
                |> (buildFilteredSearchResultBySearchType uiModel.modelFilters)
                                 (\t sf -> String.trim sf.searchFilterKey == String.trim t.model && sf.userAction == True )
                |> (buildFilteredSearchResultBySearchType uiModel.sleeperRoofFilters)
                                 (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperRoof && sf.userAction == True )
                |> (buildFilteredSearchResultBySearchType uiModel.sleeperBunkFilters)
                                 (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperBunk && sf.userAction == True )
        
        sortedFilterdTruckList =
            filterdTruckList
                |> List.sortBy .make
    in
        sortedFilterdTruckList
