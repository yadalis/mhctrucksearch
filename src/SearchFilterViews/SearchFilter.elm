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
import Helpers.Colors exposing (..)

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
                                (uiModel.transTypeFilters, "Transmission", FilterCheckBoxClicked)
                                
                            Suspension -> 
                                (uiModel.suspensionFilters, "Suspension", FilterCheckBoxClicked)
                                
                            BodyType -> 
                                (uiModel.bodyTypeFilters, "Body Type", FilterCheckBoxClicked)
                                
                            RearAxleType -> 
                                (uiModel.rearAxleTypeFilters, "Rear Axle Type", FilterCheckBoxClicked)
                            
                            TruckType -> 
                                (uiModel.truckTypeFilters, "Truck Status", FilterCheckBoxClicked)

                            FleetCode -> 
                                (uiModel.fleetCodeFilters, "Fleet Code", FilterCheckBoxClicked)
                            
                            SpecialFinancing -> 
                                (uiModel.specialFinancingFilters, "Special Financing", FilterCheckBoxClicked)
                            
                            OwningBranch -> 
                                (uiModel.owningBranchFilters, "Owning Branch", FilterCheckBoxClicked)
                            
                            LocationName -> 
                                (uiModel.locationNameFilters, "Location", FilterCheckBoxClicked)

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
                                                mhcRed
                                            else
                                                greyFont 0
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

            --buildCheckbox :  Int -> SearchFilterType -> Element Msg
            buildCheckbox :  SearchFilterType -> Element Msg
            buildCheckbox searchFilter =
                let
                    chkBoxStyle =
                                    if searchFilter.userAction then 
                                        [mhcRed, fb ]
                                    else
                                        [greyFont 0]
                    displayValue = 
                                    if searchFilter.filterCategory == TruckType then
                                        searchFilter.searchFilterExtraData
                                    else
                                        searchFilter.searchFilterKey
                    
                    --updatedSearchFilter = {searchFilter | index = index, filterCategory = searchFilterCustomType }
                in
                    if searchFilter.resultCount > 0 then
                        row[wf, size 14, pdl 25]
                        [
                            checkbox [bwb 1, wf, pdb 5, greyBorder 175 ] {
                                --onChange = msg index searchFilterCustomType searchFilter.searchFilterKey searchFilter.searchFilterExtraData --(String.trim displayValue)
                                onChange = msg searchFilter --updatedSearchFilter
                                ,icon = buildChkBoxImage
                                , label = labelRight ([centerY] ++ chkBoxStyle)  (el [] <| textValue (displayValue ++ " (" ++  (String.fromInt <| searchFilter.resultCount)  ++ ")"))
                                , checked = searchFilter.userAction
                            }
                        ]
                    else
                        none
    in
        if Array.length searchFilters > 0 then
            row[ wf, pdt 5]
            [
                column[wf]
                [
                    checkbox [] 
                    {
                        onChange = CollapseClicked searchFilterState
                        ,icon = buildCollapseAllImage
                        , label = labelRight [fs 14, showLabelRed] <|  textValue <| filterLabel
                        , checked =
                                    searchFilterState.userAction
                    }
                    ,column ( [spy 8, wf] ++ expandCollapseAll searchFilterState.userAction)
                    (
                        --Array.toList <| Array.indexedMap buildCheckbox searchFilters -- column function needs List of item and not Array of items, so need conversion
                        Array.toList <| Array.map buildCheckbox searchFilters -- column function needs List of item and not Array of items, so need conversion
                    )
                ]
            ]
        else
            none