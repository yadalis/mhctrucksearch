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

---- INIT ----

type alias OnLoadSearchFilter =
    String

init : OnLoadSearchFilter -> ( (Model, UIModel) , Cmd Msg)
init jsflg =
    ( (initialModel,initalUIModel jsflg)
        , Cmd.batch [fetchTrucks ""]
        --, Cmd.batch [fetchTrucks, fetchSearchFilterRanges] -- this executes all commands in async manner, it seems ?
    )

---- UPDATE ----
 
update : Msg -> (Model, UIModel) -> ( (Model, UIModel) , Cmd Msg  )
update msg (model, uiModel) =
    case msg of
        OnFetchSearchFilterRanges response ->
            let
                x =  Debug.log "raw json response" response

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
                
                --x =  Debug.log "raw json response" <| fetchRangeFiltersPoplulatedWithCounts Price
            in
            
                ( ( model , {uiModel | 
                                        priceFilters = fetchRangeFiltersPoplulatedWithCounts Price, 
                                        engineHPFilters = fetchRangeFiltersPoplulatedWithCounts EngineHP,
                                        sleeperInchesFilters = fetchRangeFiltersPoplulatedWithCounts SleeperInches,
                                        wheelBaseFilters = fetchRangeFiltersPoplulatedWithCounts WheelBase,
                                        mileageFilters = fetchRangeFiltersPoplulatedWithCounts Mileage,
                                        frontAxleWeightFilters = fetchRangeFiltersPoplulatedWithCounts FrontAxleWeight,
                                        rearAxleWeightFilters = fetchRangeFiltersPoplulatedWithCounts RearAxleWeight,
                                        inventoryAgeFilters = fetchRangeFiltersPoplulatedWithCounts InventoryAge
                            } ), Cmd.none)

        OnFetchTrucks response ->
            let
                
                x =  Debug.log "raw json response" response
                trucks = case response of
                            Ok truckList ->
                                    truckList
                                        |> sortTruckList uiModel.currentSortBy
                                        --|> List.take 100
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
                fleetCodeFilters = buildSearchFilterValueRecordList FleetCode uiModel.fleetCodeFilters trucks
                truckStatusFilters = buildSearchFilterValueRecordList TruckStatus uiModel.truckStatusFilters trucks
                specialFinancingFilters = buildSearchFilterValueRecordList SpecialFinancing uiModel.specialFinancingFilters trucks
                owningBranchFilters = buildSearchFilterValueRecordList OwningBranch uiModel.owningBranchFilters trucks

                -- x =  Debug.log "raw json response" sleeperBunkFilters

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
                                        fleetCodeFilters = fleetCodeFilters,
                                        truckStatusFilters = truckStatusFilters,
                                        specialFinancingFilters = specialFinancingFilters,
                                        owningBranchFilters = owningBranchFilters 
                        }
                    )
                    --, Cmd.none
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
                        FleetCode -> 
                            (uiModel.fleetCodeFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | fleetCodeFilters = mfArr})    
                        TruckStatus -> 
                            (uiModel.truckStatusFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | truckStatusFilters = mfArr})
                        SpecialFinancing -> 
                            (uiModel.specialFinancingFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | specialFinancingFilters = mfArr})                            
                        OwningBranch -> 
                            (uiModel.owningBranchFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | owningBranchFilters = mfArr})       
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

        ClearSearchStringResults ->
            ( ( {model |
                            filteredTruckList = [],
                            truckList = [],
                            pagedTruckList = []} , {uiModel | searchString = ""}), fetchTrucks "")

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
                , fetchTrucks uiModel.searchString
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
                , fetchTrucks uiModel.searchString
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
                ( ( model , {uiModel |  expandCollapseSearchFilterStates = updatedSearchFilterStates, 
                                        expandCollapseAllChecked = userAction}), Cmd.none )

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
        
        ShowTrucksWithPhotoOnly ->
            let
                photoOnlyTrucks =
                    model.truckList
                        |> List.filter (\t -> t.primaryImageLink /= "")
                        |> (\list -> 
                                    if List.length list > 0 then
                                        list
                                    else 
                                        model.filteredTruckList)
                
                newModel = {model | truckList=photoOnlyTrucks,  filteredTruckList=photoOnlyTrucks, pagedTruckList = List.take 100 photoOnlyTrucks}

                uiModelUpdatedWithLatestSearchFilters =
                         rebuildSearchFiltersBasedOnCurrentSearchCriteria newModel uiModel
            in        
                ( (newModel, uiModelUpdatedWithLatestSearchFilters), Cmd.none )

---- VIEW ----

textBox uiModel=

    Input.text [wfp 3,  bw 0
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
                            ([ bc 226 63 63, fc 250 250 250], image [hpx 32, bw one] {src = "srch_white.ico", description ="Logo" })
                        else
                            ([ bc 198 201 206, fc 245 245 245], image [hpx 32, bw one] {src = "srch_grey.ico", description ="Logo" })

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
                    row[wf,  hpx 75,  alpha  1.99, brc 97 97 97 , bw 0, pde 0 3 0 3
                     , htmlAttribute <|  style "z-index" "40", htmlAttribute <|  style "position" "fixed"
                    ]
                    [
                             column[bc 250 250 250, wfp 2, hf, bwb 1, brc 97 97 97][
                                    image [hpx 32, bw one, centerY] {src = "https://az832863.vo.msecnd.net/~/media/images/components/pagelogos/mhclogo.png?_=-381616326&h=61", description ="Logo" }
                            ]
                            ,column[pdl 25, bc 248 248 248, wf, hf, bwb 1, brc 97 97 97, fc 97 97 97][
                                    column[ bwl 2, pdl 3, brc 255 94 94, centerY]
                                        [el [fs 26 ] <| textValue "Suresh Yadali"
                                        ,el [fs 18, pdt 15 ] <| textValue "Kansas City, MO"
                                ]
                            ]
                    ] 
        in
            
                --layoutWith {options = [focusStyle]}  [pde 83 10 10 10 
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
                    --row[hf,wf, spx 25, wfmax 1920]
                    column[hf, wfmax 1920]
                    [
                        navBar,
                        row[hf,wf, pde 75 3 100 3]
                        [     
                            -- Search Filter Panel
                            column [wf,  spy 15,  bc 215 215 215, eat] 
                            [
                                row[wf, pd 3, bw 0]
                                [ 
                                    lazy textBox uiModel
                                    ,Input.button ( [hf, wpx 50, eId "submitSrch"] ++ searchStringBtnStyle)
                                        { 
                                            onPress = if String.length uiModel.searchString > 0 then Just SearchPressed else Nothing --Just SearchPressed 
                                            ,label = searchBtnIcon
                                        }
                                ]
                                ,row[centerY, bw 0,  pde 0 5 0 5, spx 75, wf ]
                                [
                                    Input.button ( [  eal, hf, pdl 3, fs 16, eId "clearSrch", bw 1, mouseOver [fc 217 98 69] , fc 0 0 0])
                                        { 
                                            onPress = Just ClearSearchStringResults
                                            ,label = el[pd 5] <| textValue "Clear Results"
                                        }
                                    ,
                                    --centerX, centerY , brc 215 23 89, bw 2
                                    checkbox [fs 16, bw 1,  hf, ear] {
                                        onChange = CollapseAllClicked
                                        ,icon =  (\chkVal -> Element.none) -- buildCollapseAllImage
                                        , label = labelLeft [centerX] (el [] <| textValue <| if uiModel.expandCollapseAllChecked then "Collapse All" else "Expand All" )
                                        , checked = uiModel.expandCollapseAllChecked
                                    }
                                ]
                                ,column[wf, spy 5, bc 240 240 240, bw 0 ]
                                    <| loaderIconElement :: List.map 
                                                (\filterType -> lazy3 buildSearchFilterValuesGroup filterType.filterName model uiModel) 
                                                allFilterTypesMasterListWithItsInitialState
                                --[
                                    --loaderIconElement

                                    -- ,if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup SalesStatus model uiModel
                                    --     ,lazy3 buildSearchFilterValuesGroup Year model uiModel
                                    -- else
                                    --     none
                                    -- ,if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup Year model uiModel
                                    -- else
                                    --     none
                                    -- ,if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup Make model uiModel
                                    -- else
                                    --     none    
                                    -- , if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup MakeModel model uiModel
                                    -- else
                                    --     none
                                    -- , if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup SleeperRoof model uiModel
                                    -- else
                                    --     none
                                    -- , if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup SleeperBunk model uiModel
                                    -- else
                                    --     none                                                            
                                    -- , if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup EngineMake model uiModel
                                    -- else
                                    --     none                                                            
                                    -- , if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup TransType model uiModel
                                    -- else
                                    --     none                                                            
                                    -- , if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup Suspension model uiModel
                                    -- else
                                    --     none                                                            
                                    -- , if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup BodyType model uiModel
                                    -- else
                                    --     none
                                    -- , if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup RearAxleType model uiModel
                                    -- else
                                    --     none                                                        
                                    -- , if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup FleetCode model uiModel
                                    -- else
                                    --     none                                                        
                                    -- , if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup TruckStatus model uiModel
                                    -- else
                                    --     none                                                        
                                    -- , if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup SpecialFinancing model uiModel
                                    -- else
                                    --     none                                                        
                                    -- , if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup OwningBranch model uiModel
                                    -- else
                                    --     none      
                                    -- , if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup Price model uiModel
                                    -- else
                                    --     none                                                        
                                    -- , if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup EngineHP model uiModel
                                    -- else
                                    --     none                                                        
                                    -- , if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup SleeperInches model uiModel
                                    -- else
                                    --     none                                                        
                                    -- , if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup WheelBase model uiModel
                                    -- else
                                    --     none                                                        
                                    -- , if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup Mileage model uiModel
                                    -- else
                                    --     none                                                        
                                    -- , if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup FrontAxleWeight model uiModel
                                    -- else
                                    --     none                                                        
                                    -- , if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup RearAxleWeight model uiModel
                                    -- else
                                    --     none                                                        
                                    -- , if List.length model.filteredTruckList > 0 then
                                    --     lazy3 buildSearchFilterValuesGroup InventoryAge model uiModel
                                    -- else
                                    --     none
                                --]
                            ]
                            
                            -- Trucks Search Result List Panel 
                            ,column[  wfp 5,  bw 0 ,pdl 15,  eat]
                            [
                                row[wf, bwb 0, hfRange 65 150 , pd 0,  bc 215 215 215, bw 0]
                                [ 
                                    column[wf , hf, bw 0]
                                    [
                                        wrappedRow [wf,  bw 0, pdl 5 , eat]
                                            -- using <| u can avoid parans around the below func and its params
                                            <| buildPageNumbersView  model.filteredTruckList model.currentPageNumber
                                    ]
                                    ,row[hf, bw 0, pdb 3,  bc 215 215 215,wpx 500]
                                    [
                                        column[wf, hf]
                                        [
                                                row[wf]
                                                [   
                                                        el [eat,ear, pdb 0, pdr 5,bw 0,  fc 219 108 98] <| textValue <| "Total trucks found : " ++ (String.fromInt <| (List.length model.filteredTruckList))   
                                                ]
                                            
                                             ,  row[bw 0, spaceEvenly, eab]
                                                [
                                                    el [ pdr 15,bw 0,  fc 97 97 97,eal
                                                        , below (showSortOptionsDialog uiModel.showDropdown uiModel.currentSortBy)
                                                    ]
                                                        <| Input.button [pdl 5, wf,    fb, fh, bwb 1  ]  
                                                            { 
                                                                onPress = Just <| OperateSortDialog <| not <| uiModel.showDropdown
                                                                ,label = textValue <| "Sort by : " ++ convertSortByToDescription uiModel.currentSortBy
                                                            }
                                                    ,column[bw 0, bwl 2, pdl 15, wf, ear]
                                                    [
                                                        el [ear, pdb 0, pdr 5,bwb 1, fc 97 97 97, onClick (ShowTrucksWithPhotoOnly), pointer] <| textValue <| "Photos only "
                                                    ]
                                                ]
                                                
                                            
                                        ]
                                    ]
                                ]
                                ,row[ wf, bwb 0, pde 5 0 5 0][
                                        lazy searchFilterBulletView 
                                                << Array.fromList <| List.concat
                                                                                [ 
                                                                                    Array.toList uiModel.salesStatusFilters,
                                                                                    Array.toList uiModel.yearFilters,
                                                                                    Array.toList uiModel.makeFilters,
                                                                                    Array.toList uiModel.modelFilters,
                                                                                    Array.toList uiModel.sleeperRoofFilters,
                                                                                    Array.toList uiModel.sleeperBunkFilters,
                                                                                    Array.toList uiModel.engineMakeFilters,
                                                                                    Array.toList uiModel.transTypeFilters,
                                                                                    Array.toList uiModel.suspensionFilters,
                                                                                    Array.toList uiModel.bodyTypeFilters,
                                                                                    Array.toList uiModel.rearAxleTypeFilters,
                                                                                    Array.toList uiModel.priceFilters,
                                                                                    Array.toList uiModel.engineHPFilters,
                                                                                    Array.toList uiModel.sleeperInchesFilters,
                                                                                    Array.toList uiModel.wheelBaseFilters,
                                                                                    Array.toList uiModel.mileageFilters,
                                                                                    Array.toList uiModel.frontAxleWeightFilters,
                                                                                    Array.toList uiModel.rearAxleWeightFilters,
                                                                                    Array.toList uiModel.fleetCodeFilters,
                                                                                    Array.toList uiModel.truckStatusFilters,
                                                                                    Array.toList uiModel.specialFinancingFilters,
                                                                                    Array.toList uiModel.inventoryAgeFilters,
                                                                                    Array.toList uiModel.owningBranchFilters
                                                                                ]
                                ]
                                ,column[ scrollbarY, wf,  bw 0, pde 5 0 0 0   ]
                                [
                                        lazy trucksView model.pagedTruckList -- model.filteredTruckList
                                ]         
                            ]
                            --Possible 3rd column to show truck details, dont need this in case of opening truck detials in a new page or show page numbers ?
                            -- ,column[bw 0, wpx 50, hf, pdl 15, pdt 87]
                            --     getPageNumbersList
                        ]                        
                    ]


buildPageNumbersView  filteredTruckList currentPageNumber = 
    let
        grps = greedyGroupsOf 100 filteredTruckList
        pageNumbers = (List.range 1  <| List.length grps)

        searchStringBtnStyle num = 
                    if currentPageNumber == num then 
                        [  bwb 0, bc 185 185 185, fc 57 57 57 , fs 16]
                    else
                        [   bwb 0, fc 244 66 95  , fs 12]
    in
    
        if List.length pageNumbers > 1 then
            List.map (\num -> 
                           row[pd 0, bw 0,wpx 30, hpx 30, pdt 10]
                                    [
                                        Input.button ([
                                                        if currentPageNumber /= num then
                                                            mouseOver [ bc  0 0 0, fc 250 250 250  ]
                                                        else
                                                            mouseOver [ bc  175 175 175 ]
                                                        ,
                                                        pd 5, wf,    fb ] ++ (searchStringBtnStyle num))
                                            { 
                                                onPress = Just (PageNumberClicked num )
                                                ,label = textValue <| String.fromInt num
                                            }
                                    ]

                    ) pageNumbers
        else
            [none]


---- PROGRAM ----


main : Program OnLoadSearchFilter (Model,UIModel) Msg
main =
    Browser.element
        { view = view
        , init = init -- to capture flags from JS
        , update = update
        , subscriptions = always Sub.none
        }