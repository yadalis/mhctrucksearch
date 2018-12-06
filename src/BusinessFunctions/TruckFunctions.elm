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
        
        hasThisTruckMatchesWithUserSelectedFilterValue filterList func = 
            filterList
                |> Array.filter func
                |> Array.length
                |> (\length  -> length > 0)

        hasSearchFilterValuesChecked searchFilters =
            searchFilters
                |> Array.filter (\sf -> sf.userAction == True) 
                |> Array.length
                |> (\length  -> length > 0)

        filterdTruckList  = 
            model.truckList
                |> (\trks ->
                        if hasSearchFilterValuesChecked uiModel.salesStatusFilters then
                            --List.filter (\t -> hasThisTruckSalesStatusMatchesWithUserSelectedSalesStatus t ) trks -- this works too
                            List.filter (\truck -> 
                                                (hasThisTruckMatchesWithUserSelectedFilterValue uiModel.salesStatusFilters) 
                                                        (\sf -> String.trim sf.searchFilterKey == String.trim truck.salesStatus && sf.userAction == True) -- partial app style
                                        )  trks 
                        else
                            trks
                    )
                |> (\trks ->
                        if hasSearchFilterValuesChecked uiModel.yearFilters then
                            --List.filter (\t -> hasThisTruckYearMatchesWithUserSelectedYear t ) trks 
                            List.filter (\truck -> 
                                                (hasThisTruckMatchesWithUserSelectedFilterValue uiModel.salesStatusFilters) 
                                                        (\sf -> String.trim sf.searchFilterKey == String.trim truck.year && sf.userAction == True) -- partial app style
                                        )  trks 
                        else
                            trks
                    )
                |> (\trks ->
                        if hasSearchFilterValuesChecked uiModel.makeFilters then
                            --List.filter (\t -> hasThisTruckMakeMatchesWithUserSelectedMake t ) trks 
                            List.filter (\truck -> 
                                                (hasThisTruckMatchesWithUserSelectedFilterValue uiModel.salesStatusFilters) 
                                                        (\sf -> String.trim sf.searchFilterKey == String.trim truck.make && sf.userAction == True) -- partial app style
                                        )  trks 
                        else
                            trks
                    )
                |> (\trks ->
                        if hasSearchFilterValuesChecked uiModel.modelFilters then
                            --List.filter (\t -> hasThisTruckModelMatchesWithUserSelectedModel t ) trks 
                            List.filter (\truck -> 
                                                (hasThisTruckMatchesWithUserSelectedFilterValue uiModel.salesStatusFilters) 
                                                        (\sf -> String.trim sf.searchFilterKey == String.trim truck.model && sf.userAction == True) -- partial app style
                                        )  trks 
                        else
                            trks
                    )
                |> (\trks ->
                        if hasSearchFilterValuesChecked uiModel.sleeperRoofFilters then
                            --List.filter (\t -> hasThisTruckSleeperRoofMatchesWithUserSelectedSleeperRoof t ) trks 
                            List.filter (\truck -> 
                                                (hasThisTruckMatchesWithUserSelectedFilterValue uiModel.salesStatusFilters) 
                                                        (\sf -> String.trim sf.searchFilterKey == String.trim truck.sleeperRoof && sf.userAction == True) -- partial app style
                                        )  trks 
                        else
                            trks
                    )
                |> (\trks ->
                        if hasSearchFilterValuesChecked uiModel.sleeperBunkFilters then
                            --List.filter (\t -> hasThisTruckSleeperBunkMatchesWithUserSelectedSleeperBunk t ) trks 
                            List.filter (\truck -> 
                                                (hasThisTruckMatchesWithUserSelectedFilterValue uiModel.salesStatusFilters) 
                                                        (\sf -> String.trim sf.searchFilterKey == String.trim truck.sleeperBunk && sf.userAction == True) -- partial app style
                                        )  trks 
                        else
                            trks
                    )
        
        sortedFilterdTruckList =
            filterdTruckList
                |> List.sortBy .make
    in
        sortedFilterdTruckList
