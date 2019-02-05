module BusinessFunctions.SearchFilterFunctions exposing (..)

import Model exposing (..)
import Array exposing (..)
import List.Extra exposing (..)
import List.Unique exposing (..)
import Maybe.Extra exposing (..)
 
resetFilters filters = 
        Array.map(\sf -> {sf | userAction = False } ) filters

anyFilterApplied uiModel =
        List.length (uiModel.selectedFilterBullets) > 0

partialSearchFiltersMetadata = 
    [
         {filterName = FleetCode,        filterCode = "fc"  ,  displayText = "Fleet Code"        }
        ,{filterName = SalesStatus,      filterCode = "ss"  ,  displayText = "Sales Status"      }
        ,{filterName = TruckType,        filterCode = "tt"  ,  displayText = "Truck Status"      }
        ,{filterName = SpecialFinancing, filterCode = "sf"  ,  displayText = "Special Financing" }
        ,{filterName = Price,            filterCode = "pr"  ,  displayText = "Price"             }
        ,{filterName = Mileage,          filterCode = "ml"  ,  displayText = "Mileage"           }
        ,{filterName = Year,             filterCode = "yr"  ,  displayText = "Year"              }
        ,{filterName = Make,             filterCode = "mk"  ,  displayText = "Make"              }
        ,{filterName = MakeModel,        filterCode = "mm"  ,  displayText = "Model"             }
        ,{filterName = EngineMake,       filterCode = "em"  ,  displayText = "Engine"            }
        ,{filterName = EngineHP,         filterCode = "eh"  ,  displayText = "HP"                }
        ,{filterName = TransType,        filterCode = "ts"  ,  displayText = "Transmission"      }
        ,{filterName = SleeperRoof,      filterCode = "sr"  ,  displayText = "Sleeper Roof"      }
        ,{filterName = SleeperBunk,      filterCode = "sb"  ,  displayText = "Sleeper Bunk"      }
        ,{filterName = SleeperInches,    filterCode = "si"  ,  displayText = "Sleeper Size"      }
        ,{filterName = Suspension,       filterCode = "su"  ,  displayText = "Suspension"        }
        ,{filterName = WheelBase,        filterCode = "wb"  ,  displayText = "Wheel Base"        }        
        ,{filterName = RearAxleType,     filterCode = "ra"  ,  displayText = "Rear Axle Type"    }
        ,{filterName = RearAxleRatio,    filterCode = "rr"  ,  displayText = "Rear Axle Ratio"   }      
        ,{filterName = FrontAxleWeight,  filterCode = "fw"  ,  displayText = "Front Axle Weight" }
        ,{filterName = RearAxleWeight,   filterCode = "rw"  ,  displayText = "Rear Axle Weight"  }
        ,{filterName = RearWheelSize,    filterCode = "rs"  ,  displayText = "Rear Wheel Size"   }      
        ,{filterName = FrontWheelSize,   filterCode = "fs"  ,  displayText = "Front Wheel Size"  }       
        ,{filterName = BrakeType,        filterCode = "bt"  ,  displayText = "Brake Type"        }
        ,{filterName = ExhaustType,      filterCode = "et"  ,  displayText = "Exhaust Type"      }
        ,{filterName = BodyType,         filterCode = "bt"  ,  displayText = "Body Type"         }
        ,{filterName = APU,              filterCode = "ap"  ,  displayText = "APU"               }
        ,{filterName = CDL,              filterCode = "cd"  ,  displayText = "CDL"               }
        ,{filterName = Photo,            filterCode = "ph"  ,  displayText = "Photo"             }      
        ,{filterName = InventoryAge,     filterCode = "ia"  ,  displayText = "Inventory Age"     }
        ,{filterName = LocationName,     filterCode = "lc"  ,  displayText = "Location Name"     }
        ,{filterName = OwningBranch,     filterCode = "ob"  ,  displayText = "Owning Branch"     }       
    ]

formattedSelectedFilterBullets uiModel = 
        if List.length uiModel.selectedFilterBullets > 0 then
        partialSearchFiltersMetadata
                |> List.map 
                                (
                                \eachFilterType -> 
                                        uiModel.selectedFilterBullets
                                                |> List.filter (\selectedFilterBullet -> selectedFilterBullet.filterCategory == eachFilterType.filterName) 
                                                |> List.map (\eachBullet -> eachBullet.searchFilterExtraData )
                                                |> String.join ","
                                                |> \finalStr -> 
                                                                if String.length finalStr > 0 then 
                                                                        eachFilterType.filterCode ++ "=" ++ finalStr
                                                                else
                                                                        ""
                                )
        else
        []

-- getSelectedFilterBulletsByFilterCategory filterCategory selectedFilterList =
--         (Array.fromList <| List.filter (\sf -> sf.filterCategory == filterCategory)  selectedFilterList)
         
-- getSelectedSearchFilterKeys searchFilters =
--     searchFilters 
--         |> Array.map (\sf -> sf.searchFilterKey) 
--         |> Array.toList

-- getSelectedSearchFilterExtraData searchFilters =
--     searchFilters 
--         |> Array.map (\sf -> sf.searchFilterExtraData) 
--         |> Array.toList

-- isGivenValueMatchesWithSelectedFilters value searchFilters  = 
--         getSelectedSearchFilterKeys searchFilters
--             |> notMember (String.trim value)
--             |> not

-- isGivenValueMatchesWithSelectedRangeFilters value searchFilters  = 
--         getSelectedSearchFilterExtraData searchFilters
--                 |> List.filter 
--                                 (
--                                     \extraDataValue ->
--                                         getMinMaxValue extraDataValue
--                                             |> (\(minValue,maxValue) ->   value >= minValue && value <= maxValue)
--                                 )
--                 |> (\filteredList -> List.length filteredList > 0)

-- returnPrevOrCurrentlyFilteredTrucks prevFilterdTruckList currentFilteredTruckList =
--         if List.length currentFilteredTruckList > 0 then
--                 currentFilteredTruckList
--         else
--                 prevFilterdTruckList

-- filterBySingleValue (selectedFilters, trucksList) filterCategory uiModel =
--         (find (\filterMetaData -> filterMetaData.filterName == filterCategory ) partialSearchFiltersMetadata )
--                 |> Maybe.map 
--                         (\fltr -> 
--                                 List.filter 
--                                         (
--                                                 (\t -> fltr.searchTrucksFunction t
--                                                         (getSelectedFilterBulletsByFilterCategory filterCategory selectedFilters) )
--                                         ) 
--                                 trucksList
--                                         |> returnPrevOrCurrentlyFilteredTrucks trucksList
--                                         |> (\trks -> (selectedFilters, trks, uiModel))
--                         )
--                 |> Maybe.withDefault (selectedFilters, trucksList, uiModel)


-- executeFilterFunc filterMetaData (sfBullets, trks, uiModel)  =
--         filterBySingleValue (sfBullets, trks) filterMetaData.filterName uiModel
        
-- rebuildFilters filterStyle filterCategory filters (selectedFilterBullets, finalFilteredTrucks, uiModel) =
--         buildSearchFilterValueRecordList filterStyle filterCategory filters finalFilteredTrucks
--                 |> Array.map
--                         (\sf ->
--                                 findMatchAndSetUserAction (Array.fromList selectedFilterBullets) sf
--                         )

-- findMatchAndSetUserAction filters sf =
--                         filters
--                                 |> Array.filter(\uiSF -> uiSF.searchFilterKey == sf.searchFilterKey && uiSF.filterCategory == sf.filterCategory)
--                                 |> Array.toList
--                                 |> List.head
--                                 |> (\headItem -> 
--                                         case headItem of 
--                                                 Just val -> val
--                                                 Nothing -> sf)
--                                 |> (\headItem -> {sf | userAction = headItem.userAction} )

-- rebuildSearchFiltersBasedOnCurrentSearchCriteria : Model -> UIModel -> UIModel
-- rebuildSearchFiltersBasedOnCurrentSearchCriteria model uiModel =
--         let 
--                 applyAllFiltersExcept filterCategory filters  filterStyle =
--                         List.foldl
--                                 executeFilterFunc
--                                 (uiModel.selectedFilterBullets, model.truckList, uiModel) 
--                                 (List.filter (\filterMetaData -> filterMetaData.filterName /= filterCategory ) partialSearchFiltersMetadata )
--                                         |> rebuildFilters filterStyle filterCategory filters
                
--                 executeRegularAndRangeFilterFunc filterMeta currentUIModel =
--                         filterMeta.pushModifiedFilterListBackInToUIModel currentUIModel (applyAllFiltersExcept  filterMeta.filterName (filterMeta.filters currentUIModel) filterMeta.filterStyle)

--                 newUIModel = 
--                         List.foldl
--                                 executeRegularAndRangeFilterFunc
--                                 uiModel
--                                 partialSearchFiltersMetadata
--         in
--                 newUIModel

-- applySearchFilters: Model -> UIModel -> List Truck
-- applySearchFilters model uiModel =
--     let
--         filterdTruckList  = 
--                 List.foldl
--                         executeFilterFunc 
--                         (uiModel.selectedFilterBullets, model.truckList, uiModel) 
--                         partialSearchFiltersMetadata
--                                 |> \(_, finalFilteredTrucks, _) -> finalFilteredTrucks
--     in
--         filterdTruckList

-- getMinMaxValue rangeString =
--         let
--                 minmaxValues = String.split "-" rangeString
--                 minValue =     
--                         case List.head <| minmaxValues of -- gives first element in the list
--                         Just strMinVal -> case String.toFloat strMinVal of 
--                                                 Just minVal -> minVal
--                                                 Nothing -> 0                    
--                         Nothing -> 0
--                 maxValue =
--                         case List.head <| List.reverse minmaxValues of -- gives last element in the list -- 2nd style
--                                 Just strMaxVal -> case String.toFloat strMaxVal of 
--                                                 Just maxVal -> maxVal
--                                                 Nothing -> 0     
--                                 Nothing -> 0     
--         in
--                 (minValue, maxValue)

-- -- simple type compares
-- desendingOrder a b =
--     case compare a b of
--         LT -> GT
--         EQ -> EQ
--         GT -> LT

-- filterEmptyValuesFromList : List String -> List String
-- filterEmptyValuesFromList  searchFilterList =
--     List.filter (
--                     \str -> 
--                         str
--                             |> String.trim
--                             |> String.isEmpty
--                             |> not 
--                 )
--                 searchFilterList

-- applyExtraOnSearchFilters  : SortOrder -> List String -> Array String
-- applyExtraOnSearchFilters sortOrder searchFilterKeyValues =
--     filterDuplicates searchFilterKeyValues
--         |> filterEmptyValuesFromList
--         |> (if sortOrder == SortASC then 
--                 List.sort 
--             else 
--                 List.sortWith desendingOrder)
--         |> Array.fromList

-- buildSearchFilterValueList : SearchFilterStyle -> SearchFilterCustomType ->  Array SearchFilterType -> List Truck -> Array SearchFilterType
-- buildSearchFilterValueList filterStyle searchFilterCustomType searchFilterTypes trucks =
--         if filterStyle == SingleValue then
--                 let
                
--                         sfMetaData =  
--                                 find (\sfMeta -> sfMeta.filterName == searchFilterCustomType) regularSearchFiltersInitialExpandState
--                                         |> Maybe.map (\sfMeta -> sfMeta)
--                                         -- the below condition should never happen unless you misspell in metadata list in model.elm file
--                                         |> Maybe.withDefault {filterName = defaultSearchFiltersMetadata.filterName, filterNameString = defaultSearchFiltersMetadata.filterNameString,  truckFieldFunction = defaultSearchFiltersMetadata.truckFieldFunction, expandByDefault = defaultSearchFiltersMetadata.expandByDefault }

--                         fieldFunc = Tuple.first sfMetaData.truckFieldFunction
--                         fieldcompareFunc = Tuple.second sfMetaData.truckFieldFunction
--                 in    
--                         (fieldFunc trucks)
--                                 |> (if searchFilterCustomType == Year then applyExtraOnSearchFilters SortDSC else applyExtraOnSearchFilters SortASC)
--                                 |> (\sfArray -> 
--                                                 Array.indexedMap (\index sf -> 
 
--                                                         SearchFilterType
--                                                                         index 
--                                                                         sf 
--                                                                         sf
--                                                                         False
--                                                                         (List.length <| ( List.filter (fieldcompareFunc sf) trucks ) ) 
--                                                                         searchFilterCustomType
--                                                 )
--                                                 sfArray
--                                 ) 
--         else
--                 let
--                         sfMetaData =  
--                                 find (\sfMeta -> sfMeta.filterName == searchFilterCustomType) rangeSearchFiltersInitialExpandState
--                                         |> Maybe.map (\sfMeta -> sfMeta)
--                                         -- the below condition should never happen unless you misspell in metadata list in model.elm file
--                                         |> Maybe.withDefault {filterName = defaultSearchFiltersMetadata.filterName, filterNameString = defaultSearchFiltersMetadata.filterNameString, truckRangeFieldFunction = defaultSearchFiltersMetadata.truckRangeFieldFunction, expandByDefault = defaultSearchFiltersMetadata.expandByDefault }

--                         fieldcompareFunc = sfMetaData.truckRangeFieldFunction
--                 in    
--                        createRangeFilters       
--                                                 searchFilterTypes
--                                                 searchFilterCustomType 
--                                                 (\minValue maxValue ->
--                                                         (List.length <| List.filter (fieldcompareFunc minValue maxValue) trucks) 
--                                                 )

-- createRangeFilters searchFilterTypes searchFilterCustomType filterCompareCheckFunc = 
--         Array.indexedMap
--                          (\index range -> 

--                             let
--                                 minmaxValue = getMinMaxValue range.searchFilterExtraData     
--                                 minValue = Tuple.first minmaxValue
--                                 maxValue = Tuple.second minmaxValue
--                             in
--                                 --using Constructor style
--                                 SearchFilterType        index
--                                                         range.searchFilterKey 
--                                                         range.searchFilterExtraData 
--                                                         False
--                                                         (filterCompareCheckFunc minValue maxValue)
--                                                         searchFilterCustomType

--                          )
--                         searchFilterTypes

-- buildSearchFilterValueRecordList : SearchFilterStyle -> SearchFilterCustomType -> Array SearchFilterType -> List Truck -> Array SearchFilterType
-- buildSearchFilterValueRecordList searchFilterStyle searchFilterCustomType searchFilterTypes trucks =
--     buildSearchFilterValueList searchFilterStyle searchFilterCustomType searchFilterTypes trucks