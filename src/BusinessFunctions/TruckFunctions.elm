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

buildSearchFilter uniqueFilterValuesFromTextSearchResult getCountFunc filterCategory =
        Array.filter(\sf -> not <| String.isEmpty sf.searchFilterKey)
                << Array.indexedMap (\index filterValue -> 
                                                SearchFilterType   
                                                                index 
                                                                filterValue 
                                                                filterValue  
                                                                False 
                                                                (getCountFunc filterValue)
                                                                filterCategory
                        ) << Array.fromList <| uniqueFilterValuesFromTextSearchResult

buildRangeSearchFilter trucks searchFilters filterCategory =
        Array.indexedMap
                (\index range -> 

                        let
                                minmaxValue = getMinMaxValue range     
                                minValue = Tuple.first minmaxValue
                                maxValue = Tuple.second minmaxValue
                        in
                                SearchFilterType   index 
                                                        range.searchFilterKey 
                                                        range.searchFilterExtraData 
                                                        False 
                                                        (count (\t -> t.price >= minValue && t.price <= maxValue) trucks) 
                                                        filterCategory
                )
                searchFilters


rebuildSearchFiltersBasedOnTextSeachResults : Model -> UIModel -> UIModel
rebuildSearchFiltersBasedOnTextSeachResults model uiModel =
        let
                
                filterRelatedFuncs = 
                        [
                                (SalesStatus, List.map (\t -> t.salesStatus), (\filterValue -> 
                                                                        count (\t -> String.trim t.salesStatus == String.trim filterValue) model.filteredTruckList) ),
                                (Year, List.map (\t -> t.year), (\filterValue -> 
                                                                        count (\t -> String.trim t.year == String.trim filterValue) model.filteredTruckList) ),
                                (Make, List.map (\t -> t.make), (\filterValue -> 
                                                                        count (\t -> String.trim t.make == String.trim filterValue) model.filteredTruckList ) ),
                                (MakeModel, List.map (\t -> t.model), (\filterValue -> 
                                                                        count (\t -> String.trim t.model == String.trim filterValue) model.filteredTruckList ) ),
                                (SleeperRoof, List.map (\t -> t.sleeperRoof), (\filterValue -> 
                                                                        count (\t -> String.trim t.sleeperRoof == String.trim filterValue) model.filteredTruckList) ),
                                (SleeperBunk, List.map (\t -> t.sleeperBunk), (\filterValue -> 
                                                                        count (\t -> String.trim t.sleeperBunk == String.trim filterValue) model.filteredTruckList ) )
                        ]

                allUpdatedFilters = List.map(\(filterCategory, filedMapFunc, countFunc) ->
                                let
                                        updatedFilters = 
                                                buildSearchFilter 
                                                        (model.filteredTruckList
                                                                |> filedMapFunc
                                                                |> filterDuplicates
                                                        )
                                                        
                                                        (
                                                                countFunc
                                                        )
                                                        filterCategory
                                in
                                        (filterCategory,updatedFilters)
                        ) filterRelatedFuncs
                        |> Debug.log "-----------"

                getDefaultSearchFilters : List (SearchFilterCustomType, Array SearchFilterType) -> Array SearchFilterType
                getDefaultSearchFilters searchFilters =
                        searchFilters
                                |> List.head
                                |> Maybe.map (\ssFilters -> Tuple.second ssFilters)
                                |> Maybe.withDefault Array.empty

                updatedSalesStatusFilters = List.filter (\(filterCategory, lst) -> filterCategory == SalesStatus ) allUpdatedFilters
                                                |> getDefaultSearchFilters
                updatedYearFilters = List.filter (\(filterCategory, lst) -> filterCategory == Year ) allUpdatedFilters
                                                |> getDefaultSearchFilters
                updatedMakeFilters = List.filter (\(filterCategory, lst) -> filterCategory == Make ) allUpdatedFilters
                                                |> getDefaultSearchFilters
                updatedModelFilters = List.filter (\(filterCategory, lst) -> filterCategory == MakeModel ) allUpdatedFilters
                                                |> getDefaultSearchFilters
                updatedSleeperRoofFilters = List.filter (\(filterCategory, lst) -> filterCategory == SleeperRoof ) allUpdatedFilters
                                                |> getDefaultSearchFilters
                updatedSleeperBunkFilters = List.filter (\(filterCategory, lst) -> filterCategory == SleeperBunk ) allUpdatedFilters
                                                |> getDefaultSearchFilters
                                                |> Debug.log "bunk filters ===================================>"
                
                -- updatedPriceFilters = List.filter (\(filterCategory, lst) -> filterCategory == Price ) allUpdatedFilters
                --                                 |> getDefaultSearchFilters
                

                -- rangeFitlerFuncs =
                --         [
                --                 (Price, List.map (\sf -> sf.asdfasdf ), (\filterValue -> 
                --                                                         count (\t -> t.price == filterValue) model.filteredTruckList ) )
                --         ]
                --         |> Debug.log "asdfasdfsadfsadfdsaf========="
                
                rangeFitlerFuncs =
                        [
                                (Price, (\filterValue -> 
                                                                        count (\t -> t.price == filterValue) model.filteredTruckList ) )
                        ]
                        |> Debug.log "asdfasdfsadfsadfdsaf========="
                        
                updatedPriceFilters = buildRangeSearchFilter model.filteredTruckList uiModel.priceFilters Price
                        --|> Debug.log "vvvvvvvvvvvvvv" 

                newUIModel ={ 
                            uiModel   | 
                                        salesStatusFilters = updatedSalesStatusFilters, 
                                        yearFilters = updatedYearFilters, 
                                        makeFilters = updatedMakeFilters, 
                                        modelFilters = updatedModelFilters, 
                                        sleeperRoofFilters = updatedSleeperRoofFilters, 
                                        sleeperBunkFilters = updatedSleeperBunkFilters,
                                        priceFilters = updatedPriceFilters
                        }
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
                model.truckList -- you need to use filteredTruckList if the result is from the TEXT search, so figure out
                                -- if the trucks returned by TEXT search or by clicking the filter check boxes
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
