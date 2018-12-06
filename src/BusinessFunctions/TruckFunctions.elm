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
        -- hasThisTruckSalesStatusMatchesWithUserSelectedSalesStatus truck = 
        --     uiModel.salesStatusFilters
        --         |> Array.filter (\sf -> String.trim sf.searchFilterKey == String.trim truck.salesStatus && sf.userAction == True) 
        --         |> Array.length
        --         |> (\length  -> length > 0)

        -- hasThisTruckYearMatchesWithUserSelectedYear truck = 
        --     uiModel.yearFilters
        --         --|> Array.filter (\sf -> Tuple.first sf == truck.year && Tuple.second sf == True)
        --         |> Array.filter (\sf -> String.trim sf.searchFilterKey == String.trim truck.year && sf.userAction == True) 
        --         |> Array.length
        --         |> (\length  -> length > 0)

        -- hasThisTruckMakeMatchesWithUserSelectedMake truck = 
        --     uiModel.makeFilters
        --         |> Array.filter (\sf -> String.trim sf.searchFilterKey == String.trim truck.make && sf.userAction == True) 
        --         |> Array.length
        --         |> (\length  -> length > 0)
        
        -- hasThisTruckModelMatchesWithUserSelectedModel truck = 
        --     uiModel.modelFilters
        --         |> Array.filter (\sf -> String.trim sf.searchFilterKey == String.trim truck.model && sf.userAction == True) 
        --         |> Array.length
        --         |> (\length  -> length > 0)

        -- hasThisTruckSleeperRoofMatchesWithUserSelectedSleeperRoof truck = 
        --     uiModel.sleeperRoofFilters
        --         |> Array.filter (\sf -> String.trim sf.searchFilterKey == String.trim truck.sleeperRoof && sf.userAction == True) 
        --         |> Array.length
        --         |> (\length  -> length > 0)

        -- hasThisTruckSleeperBunkMatchesWithUserSelectedSleeperBunk truck = 
        --     uiModel.sleeperBunkFilters
        --         |> Array.filter (\sf -> String.trim sf.searchFilterKey == String.trim truck.sleeperBunk && sf.userAction == True) 
        --         |> Array.length
        --         |> (\length  -> length > 0)

        hasAnyOfSearchFilterValuesChecked searchFilters =
            searchFilters
                |> Array.filter (\sf -> sf.userAction == True) 
                |> Array.length
                |> (\length  -> length > 0)

        hasThisTruckMatchesWithUserSelectedFilterValue filterList partialCompareWaitingForSecondParamSearchFilter = 
            filterList
                |> Array.filter partialCompareWaitingForSecondParamSearchFilter --(\sf -> comparefilterKeyValueWithTruck sf) -- you dont need to write an anonymous func with a param,
                --|> Array.filter (\sf -> comparefilterKeyValueWithTruck sf) -- same as above, but just defining filter explicitly show the param being passed
                |> Array.length
                |> (\length  -> length > 0)

        buildFilteredSearchResultBySearchType trucks filterList  comparefilterKeyValueWithTruck =
             if hasAnyOfSearchFilterValuesChecked filterList then
                List.filter (\truck -> 
                                    hasThisTruckMatchesWithUserSelectedFilterValue filterList (comparefilterKeyValueWithTruck truck) -- partial app again
                                        -- comparefilterKeyValueWithTruck needs two params truck and a searchFitler \t sf style, but we call here with just truck
                                        -- so when we call (comparefilterKeyValueWithTruck truck), it call the below line and returns a function like the last line, which
                                        --  is like waiting it to be called with the last param which is searchFilter and then the function fully executed from (Array.filter func)
                                            -- (\t sf -> String.trim sf.searchFilterKey == String.trim t.salesStatus && sf.userAction == True ) -- partial apps again !!!
                                            -- (\sf -> String.trim sf.searchFilterKey == String.trim t.salesStatus && sf.userAction == True ) -- partial apps again !!!
                                        -- which is first param and then this gets called with the second param searchFilter from 
                                            
                            )  trucks 
            else
                trucks

        filterdTruckList  = 
            model.truckList
                |> (\trks ->
                        (uiModel.salesStatusFilters
                            |> buildFilteredSearchResultBySearchType trks) -- this returns partial application and then gets executed with next line as a the last func param
                                 (\t sf -> String.trim sf.searchFilterKey == String.trim t.salesStatus && sf.userAction == True ) -- partial apps again !!!
                        -- if hasSearchFilterValuesChecked uiModel.salesStatusFilters then
                        --     --List.filter (\t -> hasThisTruckSalesStatusMatchesWithUserSelectedSalesStatus t ) trks -- this works too
                        --     List.filter (\truck -> 
                        --                         (hasThisTruckMatchesWithUserSelectedFilterValue uiModel.salesStatusFilters) 
                        --                                 (\sf -> String.trim sf.searchFilterKey == String.trim truck.salesStatus && sf.userAction == True) -- partial app style
                        --                 )  trks 
                        -- else
                        --     trks
                    )
                |> (\trks ->
                        (uiModel.yearFilters
                            |> buildFilteredSearchResultBySearchType trks)
                                 (\t sf -> String.trim sf.searchFilterKey == String.trim t.year && sf.userAction == True ) -- partial apps again !!!
                        -- if hasSearchFilterValuesChecked uiModel.yearFilters then
                        --     --List.filter (\t -> hasThisTruckYearMatchesWithUserSelectedYear t ) trks 
                        --     List.filter (\truck -> 
                        --                         (hasThisTruckMatchesWithUserSelectedFilterValue uiModel.salesStatusFilters) 
                        --                                 (\sf -> String.trim sf.searchFilterKey == String.trim truck.year && sf.userAction == True) -- partial app style
                        --                 )  trks 
                        -- else
                        --     trks
                    )
                |> (\trks ->
                        (uiModel.makeFilters
                            |> buildFilteredSearchResultBySearchType trks)
                                 (\t sf -> String.trim sf.searchFilterKey == String.trim t.make && sf.userAction == True ) -- partial apps again !!!
                        -- if hasSearchFilterValuesChecked uiModel.makeFilters then
                        --     --List.filter (\t -> hasThisTruckMakeMatchesWithUserSelectedMake t ) trks 
                        --     List.filter (\truck -> 
                        --                         (hasThisTruckMatchesWithUserSelectedFilterValue uiModel.salesStatusFilters) 
                        --                                 (\sf -> String.trim sf.searchFilterKey == String.trim truck.make && sf.userAction == True) -- partial app style
                        --                 )  trks 
                        -- else
                        --     trks
                    )
                |> (\trks ->
                        (uiModel.modelFilters
                            |> buildFilteredSearchResultBySearchType trks)
                                 (\t sf -> String.trim sf.searchFilterKey == String.trim t.model && sf.userAction == True ) -- partial apps again !!!
                        -- if hasSearchFilterValuesChecked uiModel.modelFilters then
                        --     --List.filter (\t -> hasThisTruckModelMatchesWithUserSelectedModel t ) trks 
                        --     List.filter (\truck -> 
                        --                         (hasThisTruckMatchesWithUserSelectedFilterValue uiModel.salesStatusFilters) 
                        --                                 (\sf -> String.trim sf.searchFilterKey == String.trim truck.model && sf.userAction == True) -- partial app style
                        --                 )  trks 
                        -- else
                        --     trks
                    )
                |> (\trks ->
                        (uiModel.sleeperRoofFilters
                            |> buildFilteredSearchResultBySearchType trks)
                                 (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperRoof && sf.userAction == True ) -- partial apps again !!!
                        -- if hasSearchFilterValuesChecked uiModel.sleeperRoofFilters then
                        --     --List.filter (\t -> hasThisTruckSleeperRoofMatchesWithUserSelectedSleeperRoof t ) trks 
                        --     List.filter (\truck -> 
                        --                         (hasThisTruckMatchesWithUserSelectedFilterValue uiModel.salesStatusFilters) 
                        --                                 (\sf -> String.trim sf.searchFilterKey == String.trim truck.sleeperRoof && sf.userAction == True) -- partial app style
                        --                 )  trks 
                        -- else
                        --     trks
                    )
                |> (\trks ->
                        (uiModel.sleeperBunkFilters
                            |> buildFilteredSearchResultBySearchType trks)
                                 (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperBunk && sf.userAction == True ) -- partial apps again !!!
                        -- if hasSearchFilterValuesChecked uiModel.sleeperBunkFilters then
                        --     --List.filter (\t -> hasThisTruckSleeperBunkMatchesWithUserSelectedSleeperBunk t ) trks 
                        --     List.filter (\truck -> 
                        --                         (hasThisTruckMatchesWithUserSelectedFilterValue uiModel.salesStatusFilters) 
                        --                                 (\sf -> String.trim sf.searchFilterKey == String.trim truck.sleeperBunk && sf.userAction == True) -- partial app style
                        --                 )  trks 
                        -- else
                        --     trks
                    )
        
        sortedFilterdTruckList =
            filterdTruckList
                |> List.sortBy .make
    in
        sortedFilterdTruckList
