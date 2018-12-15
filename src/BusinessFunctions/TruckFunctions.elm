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
import List.Extra exposing (..)
--import SearchFilterViews.SearchFilterRage exposing (..)


buildTruckIdNumber : Truck -> (String, String)
buildTruckIdNumber truck =
    if truck.stockNumber > 0 then 
        ("Stock#: " , "i0" ++ String.fromInt truck.stockNumber)
    else if truck.appraisalNumber > 0 then 
        ("Appraisal#: " , "A" ++ String.fromInt truck.appraisalNumber)
    else
        ("PO#: " , "P" ++ truck.poNumber)
        
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

buildSearchFilter uniqueFilterValuesFromTextSearchResult func =
        Array.indexedMap (\index filterValue -> 

                                        SearchFilterType   
                                                        index 
                                                        filterValue 
                                                        filterValue  
                                                        False 
                                                        --(count (\t -> t.make == yearValue) model.filteredTruckList) 
                                                        (func filterValue)
                                                        Year

                        ) << Array.fromList <| uniqueFilterValuesFromTextSearchResult

rebuildSearchFiltersBasedOnTextSeachResults : Model -> UIModel -> UIModel
rebuildSearchFiltersBasedOnTextSeachResults model uiModel =
        let
                
                filterRelatedFuncs = 
                        [
                                (Year, List.map (\t -> t.year), (\filterValue -> 
                                                                        count (\t -> t.year == filterValue) model.filteredTruckList) ),
                                (Make, List.map (\t -> t.make), (\filterValue -> 
                                                                        count (\t -> t.make == filterValue) model.filteredTruckList ) )
                        ]

                vars = List.map(\(filterType, mapFunc, countFunc) ->
                                let
                                        updatedFilters = 
                                                buildSearchFilter 
                                                        (model.filteredTruckList
                                                                --|> List.map (\t -> t.make)
                                                                |> mapFunc
                                                                |> filterDuplicates
                                                        )
                                                        
                                                        (
                                                                countFunc
                                                                -- \filterValue -> 
                                                                --         count (\t -> t.make == filterValue) model.filteredTruckList
                                                        )     
                                in
                                        updatedFilters
                        ) filterRelatedFuncs
                        |> Debug.log "-----------"

                -- updatedMakeFilters =
                --         buildSearchFilter 
                --                         (model.filteredTruckList
                --                                 |> List.map (\t -> t.make)
                --                                 --|> mapFunc
                --                                 |> filterDuplicates
                --                         )
                                        
                --                         (
                --                                 --countFunc
                --                                 \filterValue -> 
                --                                         count (\t -> t.make == filterValue) model.filteredTruckList
                --                         ) 


                -- uniqueYearValuesFromTextSearchResult = 
                --         model.filteredTruckList
                --                 |> List.map (\t -> t.year)
                --                 |> filterDuplicates

                -- updatedYearFilters  = 
                --         Array.indexedMap (\index yearValue -> 

                --                         SearchFilterType   
                --                                         index 
                --                                         yearValue 
                --                                         yearValue  
                --                                         False 
                --                                         (count (\t -> t.year == yearValue) model.filteredTruckList) 
                --                                         Year

                --         ) << Array.fromList <| uniqueYearValuesFromTextSearchResult
                
                newUIModel = {uiModel | yearFilters = updatedYearFilters, makeFilters = updatedMakeFilters}
                --newUIModel = {uiModel | makeFilters = updatedYearFilters}
        in
                newUIModel
        

        

rebuildSearchFiltersBasedOnCurrentSearchCriteria : Model -> UIModel -> UIModel
rebuildSearchFiltersBasedOnCurrentSearchCriteria model uiModel =
        let 
                --findMatchAndSetUserAction : Array SearchFilterType -> SearchFilterType -> SearchFilterType
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
           
                
                -- SearchFilterRangeType   index 
                --                                     range.searchFilterKey 
                --                                     range.searchFilterMinValue  
                --                                     range.searchFilterMaxValue False 
                --                                     (List.length <| List.filter (\t -> t.price >= range.searchFilterMinValue && t.price <= range.searchFilterMaxValue) trucks) 
                --                                     searchFilterRangeUnionType --using Constructor


                -- findMatchAndSetUserActionOnRange : Array SearchFilterRangeType -> SearchFilterRangeType -> SearchFilterRangeType
                -- findMatchAndSetUserActionOnRange filters sf =
                --         filters
                --                 |> Array.filter(\uiSF -> uiSF.searchFilterKey == sf.searchFilterKey)
                --                 |> Array.toList
                --                 |> List.head
                --                 |> (\headItem -> 
                --                         case headItem of 
                --                                 Just val -> val
                --                                 Nothing -> sf)
                --                 |> (\headItem ->  {sf | userAction = headItem.userAction}  )

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
                                >> (buildFilteredSearchResultBySearchType uiModel.priceFilters)
                                        (\t sf -> 
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.price >= minValue && t.price <= maxValue && sf.userAction == True 
                                        )
                                -- >> (\trks -> -- this is just to log intermediate result from with the function chains
                                --         let
                                --                y = Debug.log "sales filters - >" [trks]
                                --         in
                                --                 trks
                                -- )
                                >> buildSearchFilterValueRecordList SalesStatus uiModel.salesStatusFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.salesStatusFilters sf 
                                        )
                --x = Debug.log "sales filters - >" [updatedSalesStatusFitlerList]

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
                                >> (buildFilteredSearchResultBySearchType uiModel.priceFilters)
                                        (\t sf -> 
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.price >= minValue && t.price <= maxValue && sf.userAction == True 
                                        )
                                -- >> (\trks -> -- this is just to log intermediate result from with the function chains
                                --         let
                                --                y = Debug.log "year filters - >" [trks]
                                --         in
                                --                 trks
                                -- )
                                >> buildSearchFilterValueRecordList Year uiModel.yearFilters
                                        >> Array.map
                                                (\sf ->
                                                        findMatchAndSetUserAction uiModel.yearFilters sf 
                                                )
                
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
                                >> (buildFilteredSearchResultBySearchType uiModel.priceFilters)
                                        (\t sf -> 
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.price >= minValue && t.price <= maxValue && sf.userAction == True 
                                        )
                                >> buildSearchFilterValueRecordList Make uiModel.makeFilters
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
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.make && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperRoofFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperRoof && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperBunkFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperBunk && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.priceFilters)
                                        (\t sf -> 
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.price >= minValue && t.price <= maxValue && sf.userAction == True 
                                        )
                                >> buildSearchFilterValueRecordList MakeModel uiModel.modelFilters
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
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.make && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.modelFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.model && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperBunkFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperBunk && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.priceFilters)
                                        (\t sf -> 
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.price >= minValue && t.price <= maxValue && sf.userAction == True 
                                        )
                                >> buildSearchFilterValueRecordList SleeperRoof uiModel.sleeperRoofFilters
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
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.make && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.modelFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.model && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperRoofFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperRoof && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.priceFilters)
                                        (\t sf -> 
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.price >= minValue && t.price <= maxValue && sf.userAction == True 
                                        )
                                >> buildSearchFilterValueRecordList SleeperBunk uiModel.sleeperBunkFilters
                                >> Array.map
                                                (\sf ->
                                                        findMatchAndSetUserAction uiModel.sleeperBunkFilters sf 
                                                )
                
                updatedPriceFitlerList =
                        model.truckList
                                |> (buildFilteredSearchResultBySearchType uiModel.salesStatusFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.salesStatus && sf.userAction == True )
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
                                >> buildSearchFilterValueRecordList Price uiModel.priceFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.priceFilters sf 
                                        )
                
                newUIModel = 
                        {
                                uiModel |
                                                yearFilters = updatedYearFitlerList
                                                , salesStatusFilters = updatedSalesStatusFitlerList
                                                , makeFilters = updatedMakeFitlerList
                                                , modelFilters = updatedModelFitlerList
                                                , sleeperRoofFilters = updatedSleeperRoofFitlerList
                                                , sleeperBunkFilters = updatedSleeperBunkFitlerList
                                                , priceFilters = updatedPriceFitlerList
                        }
        in
                newUIModel

applySearchFilters: Model -> UIModel -> List Truck
applySearchFilters model uiModel =
    let
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
                        >> (buildFilteredSearchResultBySearchType uiModel.priceFilters)
                                        (\t sf -> 
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.price >= minValue && t.price <= maxValue && sf.userAction == True 
                                        )

        sortedFilterdTruckList =
            filterdTruckList
                --|> List.take 250
                --|> List.sortBy .make
    in
        sortedFilterdTruckList




sortTruckList sortBy listToSort =
                    case sortBy of 
                        PriceLowToHigh ->
                            listToSort
                                |> List.sortBy .price 
                        PriceHighToLow ->
                            listToSort
                                |> List.sortWith desendingOrderByPrice
                        MileageLowToHigh ->
                            listToSort
                                |> List.sortBy .mileage 
                        MileageHighToLow ->
                            listToSort
                                |> List.sortWith desendingOrderByMileage
                        MakeAtoZ ->
                            listToSort
                                |> List.sortBy .make     
                        MakeZtoA ->
                            listToSort
                                |> List.sortWith desendingOrderByMake
                        YearOldToNew ->
                            listToSort
                                |> List.sortBy .year     
                        YearNewToOld ->
                            listToSort
                                |> List.sortWith desendingOrderByYear
                                
defaultSortBy  =
    MakeAtoZ

defaultSortByText  =
    "Make A to Z"

sortByItemslist : List (String, String, SortBy)
sortByItemslist = 
    [
        ("priceLtoH","Price - Low to High",PriceLowToHigh),
        ("priceHtoL","Price - High to Low",PriceHighToLow),
        ("MileageLtoH","Mileage - Low to High",MileageLowToHigh),
        ("MileageHtoL","Mileage - High to Low",MileageHighToLow),
        ("MakeAtoZ","Make A to Z",MakeAtoZ),
        ("MakeZtoA","Make Z to A",MakeZtoA),
        ("YearNtoO","Year - New to Old",YearNewToOld),
        ("YearOtoN","Year - Old to New",YearOldToNew)
    ]

-- getConvertedSortByFromString : String -> SortBy
-- getConvertedSortByFromString key = 
--     sortByItemslist
--         |> List.filter (\(k, d, v) -> k == key)
--         |> List.head
--         |> Maybe.map (\(k, d, v) -> v)
--         |> Maybe.withDefault defaultSortBy

convertSortByToString sortBy =
    sortByItemslist
        |> List.filter(\(_,_, v) -> v == sortBy)
        |> List.head
        |> Maybe.map (\(k, d, v) -> d)
        |> Maybe.withDefault defaultSortByText
                

--flippedComparison a b =
desendingOrderByPrice a b =
    case compare a.price b.price of
        LT -> GT
        EQ -> EQ
        GT -> LT

--flippedComparison a b =
desendingOrderByMileage a b =
    case compare a.mileage b.mileage of
        LT -> GT
        EQ -> EQ
        GT -> LT

desendingOrderByMake a b =
    case compare a.make b.make of
        LT -> GT
        EQ -> EQ
        GT -> LT

desendingOrderByYear a b =
    case compare a.year b.year of
        LT -> GT
        EQ -> EQ
        GT -> LT
