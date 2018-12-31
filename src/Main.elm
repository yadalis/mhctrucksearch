module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Commands exposing (..)
--import RemoteData  exposing (..)
import Element exposing (..)
import Element.Input as Input exposing (..) 
import Element.Font as Font exposing (..)
import TruckViews.Truck exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import BusinessFunctions.TruckFunctions exposing (..)
import Array exposing(..)
import Html.Events.Extra as ExtraHtmlEvents
import SearchFilterViews.SearchFilter exposing (..)
import Element.Lazy as Lazy exposing(..)
import TruckViews.SearchFilterBullet exposing (..)
import List.Extra exposing (..)
import TruckViews.SortDialog exposing (..)
import Element.Events exposing (..)
import Helpers.Utils exposing (..)
import Browser.Dom exposing (..)
import BusinessFunctions.Pager exposing (..)
import Helpers.Colors exposing (..)

---- INIT ----

type alias OnLoadSearchFilter =
    String

init : OnLoadSearchFilter -> ( (Model, UIModel) , Cmd Msg)
init jsflg =
    ( (initialModel,initalUIModel jsflg)
        , Cmd.batch [getFetchURL "" False]
        --, Cmd.batch [fetchTrucks, fetchSearchFilterRanges] -- this executes all commands in async manner, it seems ?
    )

---- UPDATE ----
 
update : Msg -> (Model, UIModel) -> ( (Model, UIModel) , Cmd Msg  )
update msg (model, uiModel) =
    case msg of
        OnFetchSearchFilterRanges response ->
            let
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
                                            allRangeFilterTypesMasterList -- make sure this list is up to date with all possible range filters, this is defined in model.elm
                
                allRangeSearchFiltersWithCountsWithItsFilterType = 
                            List.map 
                                    (
                                        \(rangeFltrType, rangeFltrWithNoCountsList) ->  
                                                (rangeFltrType, (buildSearchFilterValueRecordList rangeFltrType (Array.fromList <| rangeFltrWithNoCountsList) model.truckList ))
                                    ) 
                            allRangeSearchFiltersWithNoCountsWithItsFilterType

                fetchRangeFiltersPoplulatedWithCounts fltrType = 
                            case    (find 
                                                    (
                                                        \(rangeFltrType,filtrArray)  -> 
                                                                    rangeFltrType == fltrType 
                                                    ) 
                                    allRangeSearchFiltersWithCountsWithItsFilterType) of
                                Just item -> Tuple.second item
                                Nothing -> Array.empty

                newUIModel = {uiModel | 
                                        priceFilters = fetchRangeFiltersPoplulatedWithCounts Price, 
                                        engineHPFilters = fetchRangeFiltersPoplulatedWithCounts EngineHP,
                                        sleeperInchesFilters = fetchRangeFiltersPoplulatedWithCounts SleeperInches,
                                        wheelBaseFilters = fetchRangeFiltersPoplulatedWithCounts WheelBase,
                                        mileageFilters = fetchRangeFiltersPoplulatedWithCounts Mileage,
                                        frontAxleWeightFilters = fetchRangeFiltersPoplulatedWithCounts FrontAxleWeight,
                                        rearAxleWeightFilters = fetchRangeFiltersPoplulatedWithCounts RearAxleWeight,
                                        inventoryAgeFilters = fetchRangeFiltersPoplulatedWithCounts InventoryAge
                            } 

            in
                ( ( model , newUIModel), Cmd.none)--sendMessage ( FilterCheckBoxClicked 0 SalesStatus True ) )

        OnFetchTrucks response ->
            let
                trucks = case response of
                            Ok truckList ->
                                    truckList
                                        |> sortTruckList uiModel.currentSortBy
                            Err err ->
                                    []

                salesStatusFilters = buildSearchFilterValueRecordList SalesStatus uiModel.salesStatusFilters trucks
                yearFilters = buildSearchFilterValueRecordList Year uiModel.yearFilters trucks
                makeFilters = buildSearchFilterValueRecordList Make uiModel.makeFilters trucks
                modelFilters = buildSearchFilterValueRecordList MakeModel uiModel.modelFilters trucks
                sleeperRoofFilters = buildSearchFilterValueRecordList SleeperRoof uiModel.sleeperRoofFilters trucks
                sleeperBunkFilters = buildSearchFilterValueRecordList SleeperBunk uiModel.sleeperBunkFilters trucks
                engineMakeFilters = buildSearchFilterValueRecordList EngineMake uiModel.engineMakeFilters trucks
                transTypeFilters = buildSearchFilterValueRecordList TransType uiModel.transTypeFilters trucks
                suspensionFilters = buildSearchFilterValueRecordList Suspension uiModel.suspensionFilters trucks
                bodyTypeFilters = buildSearchFilterValueRecordList BodyType uiModel.bodyTypeFilters trucks
                rearAxleTypeFilters = buildSearchFilterValueRecordList RearAxleType uiModel.rearAxleTypeFilters trucks
                truckTypeFilters = buildSearchFilterValueRecordList TruckType uiModel.truckTypeFilters trucks
                fleetCodeFilters = buildSearchFilterValueRecordList FleetCode uiModel.fleetCodeFilters trucks
                specialFinancingFilters = buildSearchFilterValueRecordList SpecialFinancing uiModel.specialFinancingFilters trucks
                owningBranchFilters = buildSearchFilterValueRecordList OwningBranch uiModel.owningBranchFilters trucks
                apuFilters = buildSearchFilterValueRecordList APU uiModel.apuFilters trucks
                cdlFilters = buildSearchFilterValueRecordList CDL uiModel.apuFilters trucks
                photoFilters = buildSearchFilterValueRecordList Photo uiModel.apuFilters trucks
                locationNameFilters = buildSearchFilterValueRecordList LocationName uiModel.locationNameFilters trucks

                pagedTruckList = List.take 100 trucks
            in
                ( 
                    (
                        {   model     | truckList = trucks,  filteredTruckList = trucks, pagedTruckList = pagedTruckList},
                        { 
                            uiModel   | 
                                        yearFilters = yearFilters, 
                                        makeFilters = makeFilters, 
                                        modelFilters = modelFilters, 
                                        salesStatusFilters = salesStatusFilters, 
                                        sleeperRoofFilters = sleeperRoofFilters, 
                                        sleeperBunkFilters = sleeperBunkFilters, 
                                        engineMakeFilters = engineMakeFilters,
                                        transTypeFilters = transTypeFilters,
                                        suspensionFilters = suspensionFilters,
                                        bodyTypeFilters = bodyTypeFilters,
                                        rearAxleTypeFilters = rearAxleTypeFilters,
                                        truckTypeFilters = truckTypeFilters,
                                        fleetCodeFilters = fleetCodeFilters,
                                        specialFinancingFilters = specialFinancingFilters,
                                        owningBranchFilters = owningBranchFilters,
                                        apuFilters = apuFilters,
                                        cdlFilters = cdlFilters,
                                        photoFilters = photoFilters,
                                        locationNameFilters = locationNameFilters

                        }
                    )
                    , fetchSearchFilterRanges   -- change this, otherwise it will bring all json based range filters data again and again, you should only rebuild the range filter counts
                                                -- but not regenrate the filters completely
                ) 

        FilterCheckBoxClicked index searchFilterCustomType userAction ->
            let
                updateUserSelectedSearchFilter : Array SearchFilterType -> (Array SearchFilterType -> UIModel) -> UIModel -- Anonymous funcs
                updateUserSelectedSearchFilter  filterList pushModifiedFilterListBackInToUIModel =
                    filterList
                        |> Array.get index
                        |> Maybe.map (\mf -> { mf | userAction = userAction} )
                        |> Maybe.map (\mf -> Array.set index mf filterList)
                        |> Maybe.map pushModifiedFilterListBackInToUIModel
                        |> Maybe.withDefault uiModel

                newUIModel = 
                    case searchFilterCustomType of
                        SalesStatus -> 
                            (updateUserSelectedSearchFilter <| uiModel.salesStatusFilters) (\mfArr -> {uiModel | salesStatusFilters = mfArr}) 
                        Year -> 
                            (uiModel.yearFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | yearFilters = mfArr})                                
                        Make -> 
                            (uiModel.makeFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | makeFilters = mfArr})
                        MakeModel -> 
                            (uiModel.modelFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | modelFilters = mfArr})
                        SleeperRoof -> 
                            (uiModel.sleeperRoofFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | sleeperRoofFilters = mfArr})
                        SleeperBunk -> 
                            (uiModel.sleeperBunkFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | sleeperBunkFilters = mfArr})
                        EngineMake -> 
                            (uiModel.engineMakeFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | engineMakeFilters = mfArr})    
                        TransType -> 
                            (uiModel.transTypeFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | transTypeFilters = mfArr})    
                        Suspension -> 
                            (uiModel.suspensionFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | suspensionFilters = mfArr})  
                        BodyType -> 
                            (uiModel.bodyTypeFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | bodyTypeFilters = mfArr})    
                        RearAxleType -> 
                            (uiModel.rearAxleTypeFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | rearAxleTypeFilters = mfArr})
                        TruckType -> 
                            (uiModel.truckTypeFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | truckTypeFilters = mfArr})
                        FleetCode -> 
                            (uiModel.fleetCodeFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | fleetCodeFilters = mfArr})    
                        SpecialFinancing -> 
                            (uiModel.specialFinancingFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | specialFinancingFilters = mfArr})                            
                        LocationName -> 
                            (uiModel.locationNameFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | locationNameFilters = mfArr})  
                        OwningBranch -> 
                            (uiModel.owningBranchFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | owningBranchFilters = mfArr})       
                        APU -> 
                            (uiModel.apuFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | apuFilters = mfArr})
                        CDL -> 
                            (uiModel.cdlFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | cdlFilters = mfArr})
                        Photo -> 
                            (uiModel.photoFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | photoFilters = mfArr})
                        Price -> 
                            (uiModel.priceFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | priceFilters = mfArr})    
                        EngineHP -> 
                            (uiModel.engineHPFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | engineHPFilters = mfArr})    
                        SleeperInches -> 
                            (uiModel.sleeperInchesFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | sleeperInchesFilters = mfArr})    
                        WheelBase -> 
                            (uiModel.wheelBaseFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | wheelBaseFilters = mfArr})        
                        Mileage -> 
                            (uiModel.mileageFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | mileageFilters = mfArr})        
                        FrontAxleWeight -> 
                            (uiModel.frontAxleWeightFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | frontAxleWeightFilters = mfArr})        
                        RearAxleWeight -> 
                            (uiModel.rearAxleWeightFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | rearAxleWeightFilters = mfArr})
                        InventoryAge -> 
                            (uiModel.inventoryAgeFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | inventoryAgeFilters = mfArr})    

                newSortedFilteredTruckList = applySearchFilters model newUIModel
                                            |> sortTruckList uiModel.currentSortBy

                uiModelUpdatedWithLatestSearchFilters =
                        rebuildSearchFiltersBasedOnCurrentSearchCriteria model newUIModel
            in
                ( ( {model | filteredTruckList = newSortedFilteredTruckList, pagedTruckList = List.take 100 newSortedFilteredTruckList, currentPageNumber = 1 } , uiModelUpdatedWithLatestSearchFilters), Cmd.none )

        SearchString searchString ->
                ( ( model , {uiModel | searchString = searchString}), Cmd.none)

        SearchPressed ->
            (
                (
                    {model |
                            filteredTruckList = [],
                            truckList = [],
                            pagedTruckList = []}
                    ,uiModel

                )
                , getFetchURL uiModel.searchString uiModel.workWithAppraisedTrucks
            )
            
        HandleKeyboardEvent ->
            (
                (
                    {model |
                            filteredTruckList = [],
                            truckList = [],
                            pagedTruckList = []}
                    ,uiModel

                )
                , 
                    getFetchURL uiModel.searchString uiModel.workWithAppraisedTrucks
            )
        
        CollapseClicked searchFilterState userAction->
            let
                newSearchFilterState = {searchFilterState | userAction = userAction }
                updatedSearchFilterStates = 
                    uiModel.expandCollapseSearchFilterStates
                        |> Array.set searchFilterState.index newSearchFilterState
            in
                ( ( model , {uiModel |  expandCollapseSearchFilterStates = updatedSearchFilterStates}), Cmd.none )

        CollapseAllClicked userAction ->
            let
                updatedSearchFilterStates = 
                    uiModel.expandCollapseSearchFilterStates
                        |> Array.map (\item -> {item | userAction = userAction})

            in
                ( ( model , {uiModel |  expandCollapseSearchFilterStates = updatedSearchFilterStates}), Cmd.none )

        PageNumberClicked pageNumber ->
            let              
                grps = greedyGroupsOf 100 model.filteredTruckList
                grpByPageNumber = List.drop (pageNumber - 1) grps

                firstList = case List.head grpByPageNumber of
                                    Just page -> page
                                    Nothing -> []
               
            in
                ( ( {model | pagedTruckList = firstList, currentPageNumber = pageNumber } , uiModel ), Cmd.none )

        OperateSortDialog show ->
            ( (model, {uiModel | showDropdown = show }), Cmd.none )
        
        SortTrucks sortBy ->
            let
                sortedFilteredTruckList = 
                    sortTruckList sortBy <| model.filteredTruckList 

                newModel =
                    {model | filteredTruckList = sortedFilteredTruckList, pagedTruckList = List.take 100 sortedFilteredTruckList, currentPageNumber = 1 }

            in
                ( (newModel, {uiModel | currentSortBy = sortBy}), Cmd.none )

        WorkWithAppraisedTrucks userAction ->
                        ( ( {model |
                            filteredTruckList = [],
                            truckList = [],
                            pagedTruckList = []} , {uiModel | searchString = "", workWithAppraisedTrucks = userAction}), 
                                                    getFetchURL "" userAction
                                                )

        ClearAllFilters ->
            let
                newUIModel = 
                    {
                        uiModel |
                                salesStatusFilters = resetFilters uiModel.salesStatusFilters
                                ,yearFilters = resetFilters uiModel.yearFilters
                                ,makeFilters = resetFilters uiModel.makeFilters
                                ,modelFilters = resetFilters uiModel.modelFilters
                                ,sleeperRoofFilters = resetFilters uiModel.sleeperRoofFilters
                                ,sleeperBunkFilters = resetFilters uiModel.sleeperBunkFilters
                                ,engineMakeFilters = resetFilters uiModel.engineMakeFilters
                                ,transTypeFilters = resetFilters uiModel.transTypeFilters
                                ,suspensionFilters = resetFilters uiModel.suspensionFilters
                                ,bodyTypeFilters = resetFilters uiModel.bodyTypeFilters
                                ,rearAxleTypeFilters = resetFilters uiModel.rearAxleTypeFilters
                                ,truckTypeFilters = resetFilters uiModel.truckTypeFilters
                                ,fleetCodeFilters = resetFilters uiModel.fleetCodeFilters
                                ,specialFinancingFilters = resetFilters uiModel.specialFinancingFilters
                                ,owningBranchFilters = resetFilters uiModel.owningBranchFilters
                                ,apuFilters = resetFilters uiModel.apuFilters
                                ,cdlFilters = resetFilters uiModel.cdlFilters
                                ,photoFilters = resetFilters uiModel.photoFilters
                                ,priceFilters = resetFilters uiModel.priceFilters
                                ,engineHPFilters = resetFilters uiModel.engineHPFilters
                                ,sleeperInchesFilters = resetFilters uiModel.sleeperInchesFilters
                                ,wheelBaseFilters = resetFilters uiModel.wheelBaseFilters
                                ,mileageFilters = resetFilters uiModel.mileageFilters
                                ,frontAxleWeightFilters = resetFilters uiModel.frontAxleWeightFilters
                                ,rearAxleWeightFilters = resetFilters uiModel.rearAxleWeightFilters
                                ,inventoryAgeFilters = resetFilters uiModel.inventoryAgeFilters
                                ,locationNameFilters = resetFilters uiModel.locationNameFilters
                                
                    }
                
                sortedTrkList = model.truckList |> sortTruckList uiModel.currentSortBy

                newModel = 
                    {model |
                            filteredTruckList = sortedTrkList,
                            pagedTruckList =List.take 100 sortedTrkList
                            ,currentPageNumber = 1}

                uiModelUpdatedWithLatestSearchFilters =
                        rebuildSearchFiltersBasedOnCurrentSearchCriteria newModel newUIModel
            in
                ( (newModel, uiModelUpdatedWithLatestSearchFilters), Cmd.none )

---- VIEW ----

textBox uiModel=

    Input.text
    [ wpx 300
                --,Element.htmlAttribute ( on "keydown" (Decode.map HandleKeyboardEvent  decodeKeyboardEvent) )
                , Element.htmlAttribute(ExtraHtmlEvents.onEnter HandleKeyboardEvent)
    ]
    {
        onChange = SearchString
        ,text  = uiModel.searchString
        ,label = labelLeft [] none
        ,placeholder = Just (Input.placeholder [fs 14] (el [centerY] <| textValue "Fluid truck Search"))
    }

view : (Model, UIModel) -> Html Msg
view (model, uiModel) =
        let
            (searchStringBtnStyle, searchBtnIcon) = 
                        if String.length (String.trim <| uiModel.searchString) > 0 then 
                            ([ bc 226 63 63, fc 250 250 250],  image [] {src = "srch_white.ico", description ="Logo" })
                        else
                            ([ bc 198 201 206, fc 245 245 245], image [ ] {src = "srch_grey.ico", description ="Logo" })

            loaderIconElement = 
                    if List.length model.filteredTruckList > 0 then
                        none
                    else
                        image [hpx 18, bw one, wf, pdl 5, bwb 2, eat] {src = "loader.gif", description ="Logo" }  

            focusStyle : Element.Option
            focusStyle =
                Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
        
            navBar =
                column[wf,  htmlAttribute <|  style "z-index" "40", htmlAttribute <|  style "position" "fixed", alpha  1.95, spy 0]
                [
                     -- logo row
                    row[wf, spx 15, greyBg 245]
                    [
                        row[wpx 305]
                        [
                            image [hpx 35] {src = "mhclogo.png", description ="Logo" }
                        ]
                        ,row[wf]
                        [
                            row[wpx 350, bw 1, greyBorder 200]
                            [   
                                -- you need bw 0 here to remove the border around textbox to make it blend with search button
                                Input.text[bw 0, Element.htmlAttribute(ExtraHtmlEvents.onEnter HandleKeyboardEvent)]
                                {
                                    onChange = SearchString
                                    ,text  = uiModel.searchString
                                    ,label = labelLeft [] none
                                    ,placeholder = Just (Input.placeholder [fs 14] (el [eacy] <| textValue "Fluid truck Search"))
                                }
                                ,
                                Input.button ([hf] ++ searchStringBtnStyle)
                                { 
                                    onPress = if String.length uiModel.searchString > 0 then Just SearchPressed else Nothing --Just SearchPressed 
                                    ,label = el[pde 0 5 0 5] <| textValue "SEARCH"-- searchBtnIcon
                                }
                            ]
                            ,row[ear, spx 15, fs 18]
                            [

                                checkbox [] 
                                {
                                    onChange = WorkWithAppraisedTrucks
                                    ,icon = buildWorkWithAppraisedTrucksToggleImage
                                    , label = labelRight [] <| 
                                                                if  uiModel.workWithAppraisedTrucks then 
                                                                    textValue <| "Stop Browsing Appraised Trucks"
                                                                else
                                                                    textValue <| "Browse Appraised Trucks"
                                    , checked =
                                                uiModel.workWithAppraisedTrucks
                                }
                                ,
                                row[bwl 1, hpx 30][]    
                                ,
                                row [wpx 325, fc 97 97 97, below (showSortOptionsDialog uiModel.showDropdown uiModel.currentSortBy)]
                                [
                                    Input.button []  
                                    { 
                                        onPress = Just <| OperateSortDialog <| not <| uiModel.showDropdown
                                        ,label = el[bwb 1] <| textValue <| "Sort by : " ++ convertSortByToDescription uiModel.currentSortBy
                                    }
                                ]                         
                            ]
                        ]
                    ]
                    -- exp/col/clearfitlers/totaltrucks/sort row
                    ,row[wf, spx 15, greyBg 235, bwb 1, hfRange 40 65, clipY]
                    [
                        --exp/coll/clearfitlers
                        row[wpx 300, spx 15, fs 12, mhcRed, pdl 15][
                            Input.button ( [ mouseOver [fc 217 98 69] ])
                            { 
                                onPress = Just <| CollapseAllClicked True
                                ,label = el[  bwb 1] <| textValue  "EXPAND ALL"
                            }
                            ,
                            Input.button ( [ mouseOver [fc 217 98 69]])
                            { 
                                onPress = Just <| CollapseAllClicked False
                                ,label = el[  bwb 1] <| textValue "COLLAPSE ALL"
                            }
                            ,
                            Input.button ( [ mouseOver [fc 217 98 69] ])
                            { 
                                onPress =  if anyFilterApplied uiModel then Just <| ClearAllFilters else  Nothing
                                ,label = el[  bwb 1] <| textValue "CLEAR FILTERS"
                            }
                        ]
                        ,
                        -- pager/totaltrucks-found
                        row[wf]
                        [ 
                            row[wfp 2] --clipY cuts the content of pager number if it goes beyond 65 height, this could happen
                            --if user resize the browser to a smaller width/height
                            [
                                wrappedRow [wf, pd 8 , spx 5, spy 5]
                                    -- using <| u can avoid parans around the below func and its params
                                    <| buildPageNumbersView  model.filteredTruckList model.currentPageNumber
                            ]
                            ,
                            el [wf, mhcRed] <| textValue <| "Total trucks found : " ++ (String.fromInt <| (List.length model.filteredTruckList))   
                        ]
                    ]
                ]
        in
                layoutWith {options = [focusStyle]}  [ 
                                            Font.family
                                                [ Font.external
                                                    { name = "Roboto"
                                                    , url = "https://fonts.googleapis.com/css?family=Roboto"
                                                    }
                                                , Font.sansSerif
                                                ] 
                                        ] --  inFront navBar is to make menu bar fixed
                -- [ hf, inFront navBar ] use must put hf in the array to make the scrollbarY work, otherwise screen just exaands
                -- in mormal web style and user has to scroll up and down the page
                <|
                    column[wfmax 1920]
                    [
                        navBar,
                        
                        row[wf, pde 90 3 0 3, spx 16]
                        [     
                            -- Search Filter Panel
                            column [wpx 300, eat] 
                            [
                                column[wf, spy 5, greyBg 240, pd 10]
                                    <| (
                                        if List.length model.filteredTruckList == 0 then
                                            [loaderIconElement]
                                        else 
                                            List.map 
                                                (\filterType -> lazy3 buildSearchFilterValuesGroup filterType.filterName model uiModel) 
                                                allFilterTypesMasterListWithItsInitialState
                                    )
                            ]
                            -- Trucks search Filter Bullets & Search Result List Panel 
                            ,column[wf]
                            [
                                lazy searchFilterBulletView 
                                        << Array.fromList <| concatAllFilters uiModel
                                ,
                                column[wf]
                                [
                                        lazy trucksView model.pagedTruckList -- model.filteredTruckList
                                ]         
                            ]
                            --Possible 3rd column to show truck details, dont need this in case of opening truck detials in a new page or show page numbers ?
                            -- ,column[bw 0, wpx 50, hf, pdl 15, pdt 87]
                            --     getPageNumbersList
                        ]                        
                    ]

---- PROGRAM ----


main : Program OnLoadSearchFilter (Model,UIModel) Msg
main =
    Browser.element
        { view = view
        , init = init -- to capture flags from JS
        , update = update
        , subscriptions = always Sub.none
        }