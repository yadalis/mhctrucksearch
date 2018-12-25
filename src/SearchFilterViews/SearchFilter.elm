module SearchFilterViews.SearchFilter exposing (..)

import Element exposing (..)
import Element.Input exposing (..)
import Element.Font exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Model exposing (..)
import Msg exposing (..)
import List.Unique exposing (..)
import Array exposing (..)

getMinMaxValue rangeString =
        let
                minmaxValues = String.split "-" rangeString
                minValue =     
                        case List.head <| minmaxValues of -- gives first element in the list
                        Just strMinVal -> case String.toFloat strMinVal of 
                                                Just minVal -> minVal
                                                Nothing -> 0                    
                        Nothing -> 0
                maxValue =
                        case List.head <| List.reverse minmaxValues of -- gives last element in the list -- 2nd style
                                Just strMaxVal -> case String.toFloat strMaxVal of 
                                                Just maxVal -> maxVal
                                                Nothing -> 0     
                                Nothing -> 0     
        in
                (minValue, maxValue)

-- simple type compares
desendingOrder a b =
    case compare a b of
        LT -> GT
        EQ -> EQ
        GT -> LT

filterEmptyValuesFromList : List String -> List String
filterEmptyValuesFromList  searchFilterList =
    List.filter (
                    \str -> 
                        str
                            |> String.trim
                            |> String.isEmpty
                            |> not 
                )
                searchFilterList

applyExtraOnSearchFilters  : SortOrder -> List String -> Array String
applyExtraOnSearchFilters sortOrder searchFilterKeyValue =
    filterDuplicates searchFilterKeyValue
        |> filterEmptyValuesFromList
        |> (if sortOrder == SortASC then 
                List.sort 
            else 
                List.sortWith desendingOrder)
        |> Array.fromList

buildSearchFilterValueList : SearchFilterCustomType ->  Array SearchFilterType -> List Truck -> Array SearchFilterType
buildSearchFilterValueList searchFilterCustomType searchFilterTypes trucks =
    case searchFilterCustomType of
        SalesStatus -> 
            --List.map (\t -> t.salesStatus) trucks
            List.map .salesStatus trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                               SearchFilterType index sf "EXD" 
                                               --(if String.toLower sf == "available" then True else False)
                                               False
                                               (List.length <| (List.filter (\t -> String.trim t.salesStatus == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    ) 

        Year -> 
            --List.map (\t -> t.year) trucks
            List.map .year trucks
                |> applyExtraOnSearchFilters SortDSC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.year == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )
                
        Make -> 
            --List.map (\t -> t.make) trucks
            List.map .make trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf ->  
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.make == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                

        MakeModel -> 
            --List.map (\t -> t.model) trucks
            List.map .model trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.model == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                

        SleeperRoof -> 
            --List.map (\t -> t.sleeperRoof) trucks
            List.map .sleeperRoof trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.sleeperRoof == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                
                
        SleeperBunk ->             
            List.map .sleeperBunk trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.sleeperBunk == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    ) 
                
        EngineMake ->             
            List.map .engineMake trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.engineMake == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                                
                
        TransType ->             
            List.map .transType trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.transType == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                                               
                
        Suspension ->             
            List.map .suspension trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.suspension == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                                                              
                
        BodyType ->             
            List.map .bodyType trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.bodyType == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                        
                
        RearAxleType ->             
            List.map .rearAxleType trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.rearAxleType == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                
                
        FleetCode ->        
            List.map .fleetCode trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.fleetCode == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                
                
        TruckStatus ->        
            List.map .truckStatus trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.truckStatus == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                
                
        SpecialFinancing ->        
            List.map .specialFinancing trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.specialFinancing == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                
                
        OwningBranch ->        
            List.map .owningBranch trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.owningBranch == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )      

        APU ->        
            List.map .apu trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.apu == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )  

        CDL ->        
            List.map .cdl trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.cdl == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )  

        Photo ->        
            List.map .hasPhoto trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.hasPhoto == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )  

------------------Range filters 
        Price ->            
                createRangeFilters  searchFilterTypes
                                    searchFilterCustomType 
                                    (\minValue maxValue ->
                                            (List.length <| List.filter (\t -> t.price >= minValue && t.price <= maxValue) trucks) 
                                    )

        EngineHP ->
                createRangeFilters  searchFilterTypes 
                                    searchFilterCustomType 
                                    (\minValue maxValue ->
                                            (List.length <| List.filter (\t -> t.engineHP >= minValue && t.engineHP <= maxValue) trucks) 
                                    )

        SleeperInches ->
                createRangeFilters  searchFilterTypes 
                                    searchFilterCustomType 
                                    (\minValue maxValue ->
                                            (List.length <| List.filter (\t -> t.sleeperInches >= minValue && t.sleeperInches <= maxValue) trucks) 
                                    )

        WheelBase ->
                createRangeFilters  searchFilterTypes 
                                    searchFilterCustomType 
                                    (\minValue maxValue ->
                                            (List.length <| List.filter (\t -> t.wheelBase >= minValue && t.wheelBase <= maxValue) trucks) 
                                    )

        Mileage ->
                createRangeFilters  searchFilterTypes 
                                    searchFilterCustomType 
                                    (\minValue maxValue ->
                                            (List.length <| List.filter (\t -> t.mileage >= minValue && t.mileage <= maxValue) trucks) 
                                    )

        FrontAxleWeight ->
                createRangeFilters  searchFilterTypes 
                                    searchFilterCustomType 
                                    (\minValue maxValue ->
                                            (List.length <| List.filter (\t -> t.frontAxleWeight >= minValue && t.frontAxleWeight <= maxValue) trucks) 
                                    )

        RearAxleWeight ->
                createRangeFilters  searchFilterTypes 
                                    searchFilterCustomType 
                                    (\minValue maxValue ->
                                            (List.length <| List.filter (\t -> t.rearAxleWeight >= minValue && t.rearAxleWeight <= maxValue) trucks) 
                                    )

        InventoryAge ->
                createRangeFilters  searchFilterTypes 
                                    searchFilterCustomType 
                                    (\minValue maxValue ->
                                            (List.length <| List.filter (\t -> t.inventoryAge >= minValue && t.inventoryAge <= maxValue) trucks) 
                                    )

createRangeFilters searchFilterTypes searchFilterCustomType filterCompareCheckFunc = 
        Array.indexedMap
                         (\index range -> 

                            let
                                minmaxValue = getMinMaxValue range.searchFilterExtraData     
                                minValue = Tuple.first minmaxValue
                                maxValue = Tuple.second minmaxValue
                            in
                                --using Constructor style
                                SearchFilterType   index 
                                                        range.searchFilterKey 
                                                        range.searchFilterExtraData 
                                                        False
                                                        (filterCompareCheckFunc minValue maxValue)
                                                        searchFilterCustomType

                         )
                        searchFilterTypes

buildSearchFilterValueRecordList : SearchFilterCustomType -> Array SearchFilterType -> List Truck -> Array SearchFilterType
buildSearchFilterValueRecordList searchFilterCustomType searchFilterTypes trucks =
    buildSearchFilterValueList searchFilterCustomType searchFilterTypes trucks

buildSearchFilterValuesGroup : SearchFilterCustomType ->  Model -> UIModel -> Element Msg
buildSearchFilterValuesGroup searchFilterCustomType model uiModel =
    let
            (searchFilters, filterLabel, msg)
                =   case searchFilterCustomType of
                            SalesStatus -> 
                                (uiModel.salesStatusFilters, "Sales Status", FilterCheckBoxClicked)

                            Year -> 
                                (uiModel.yearFilters, "Year", FilterCheckBoxClicked)

                            Make -> 
                                (uiModel.makeFilters, "Make", FilterCheckBoxClicked)

                            MakeModel -> 
                                (uiModel.modelFilters, "Model", FilterCheckBoxClicked)

                            SleeperRoof -> 
                                (uiModel.sleeperRoofFilters, "Sleeper Roof", FilterCheckBoxClicked)
                                
                            SleeperBunk -> 
                                (uiModel.sleeperBunkFilters, "Sleeper Bunk", FilterCheckBoxClicked)
                                
                            EngineMake -> 
                                (uiModel.engineMakeFilters, "Engine", FilterCheckBoxClicked)
                                
                            TransType -> 
                                (uiModel.transTypeFilters, "Trans Type", FilterCheckBoxClicked)
                                
                            Suspension -> 
                                (uiModel.suspensionFilters, "Suspension", FilterCheckBoxClicked)
                                
                            BodyType -> 
                                (uiModel.bodyTypeFilters, "Body Type", FilterCheckBoxClicked)
                                
                            RearAxleType -> 
                                (uiModel.rearAxleTypeFilters, "Rear Axle Type", FilterCheckBoxClicked)
                            
                            FleetCode -> 
                                (uiModel.fleetCodeFilters, "Fleet Code", FilterCheckBoxClicked)
                            
                            TruckStatus -> 
                                (uiModel.truckStatusFilters, "Truck Status", FilterCheckBoxClicked)
                            
                            SpecialFinancing -> 
                                (uiModel.specialFinancingFilters, "Special Financing", FilterCheckBoxClicked)
                            
                            OwningBranch -> 
                                (uiModel.owningBranchFilters, "Owning Branch", FilterCheckBoxClicked)
                            
                            APU -> 
                                (uiModel.apuFilters, "APU", FilterCheckBoxClicked)

                            CDL -> 
                                (uiModel.cdlFilters, "CDL", FilterCheckBoxClicked)

                            Photo -> 
                                (uiModel.photoFilters, "Show With Photos", FilterCheckBoxClicked)

                            Price -> 
                                (uiModel.priceFilters, "Price", FilterCheckBoxClicked)
                            
                            EngineHP -> 
                                (uiModel.engineHPFilters, "HP", FilterCheckBoxClicked)
                            
                            SleeperInches -> 
                                (uiModel.sleeperInchesFilters, "Sleeper Size", FilterCheckBoxClicked)
                            
                            WheelBase -> 
                                (uiModel.wheelBaseFilters, "Wheel Base", FilterCheckBoxClicked)
                            
                            Mileage -> 
                                (uiModel.mileageFilters, "Mileage", FilterCheckBoxClicked)
                            
                            FrontAxleWeight -> 
                                (uiModel.frontAxleWeightFilters, "Front Axle Weight", FilterCheckBoxClicked)
                            
                            RearAxleWeight -> 
                                (uiModel.rearAxleWeightFilters, "Rear Axle Weight", FilterCheckBoxClicked)
                            
                            InventoryAge -> 
                                (uiModel.inventoryAgeFilters, "Inventory Age", FilterCheckBoxClicked)

            showLabelRed = searchFilters
                                |> Array.toList
                                |> List.any (\sf -> sf.userAction)
                                |> (\isAnyFilterChecked -> 
                                            if isAnyFilterChecked then
                                                fc 190 5 30
                                            else
                                                fc 0 0 0 
                                    )
            
            searchFilterState = 
                    uiModel.expandCollapseSearchFilterStates
                            |> Array.filter (\mf -> mf.searchFilterCustomType == searchFilterCustomType)
                            |> Array.toList
                            |> List.head
                            |> (\possbileFirstItem ->
                                    case possbileFirstItem of
                                            Just val -> val
                                            Nothing -> SearchFilterState -1 SalesStatus False -- Nothing case will never happen, but elm forces to handle all possibel cases
                                )

            buildCheckboxes :  Int -> SearchFilterType -> Element Msg
            buildCheckboxes index searchFilter =
                let
                    chkBoxStyle =
                                    if searchFilter.userAction then 
                                        [fc  190 5 30, fb ]
                                    else
                                        [fc 0 0 0     ]
                in
                    if searchFilter.resultCount > 0 then
                        row[bw two, size 14, pdl 25]
                        [
                            checkbox [bw one, pdr 0 ] {
                                onChange = msg index searchFilterCustomType
                                ,icon = buildChkBoxImage
                                , label = labelRight ([centerY] ++ chkBoxStyle)  (el [] <| textValue searchFilter.searchFilterKey )
                                , checked = searchFilter.userAction
                            }
                            , textValue <| " (" ++  (String.fromInt <| searchFilter.resultCount)  ++ ")"
                        ]
                    else
                        none
    in
        if Array.length searchFilters > 0 then
            row[ wf, bw 0, pdt 5]
            [
                column[spy 0, wf,  bw one]
                [
                    row[bw 0,  bwb 0, wf, pdb 1,pdt 0, brc 195 195 195]
                    [
                        
                        checkbox [wf, far , bw 0] {
                                    onChange = CollapseClicked searchFilterState
                                    ,icon = buildCollapseAllImage
                                    , label = labelRight [fs 14, bwb 0, wf, fal, showLabelRed] <|  textValue <| filterLabel
                                    , checked =
                                                searchFilterState.userAction
                                }
                        
                        --,
                        -- column[wf, hf,  bw 0][
                        --     paragraph [eacx, eacy,bw one, fal, wf, hpx 25, pd 5][textValue <| filterLabel]
                        -- ]
                        -- ,column[pdr 5][
                            
                        -- ]
                    ]
                    ,column ( [spy 8, wf] ++ expandCollapseAll searchFilterState.userAction)
                    (
                        Array.toList <| Array.indexedMap buildCheckboxes searchFilters -- column function needs List of item and not Array of items, so need conversion
                    )
                ]
            ]
        else
            none