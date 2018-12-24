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
                
                -- newSortedFilteredTruckList = applySearchFilters model newUIModel
                --                             |> sortTruckList uiModel.currentSortBy

                -- newModel = {model | filteredTruckList = newSortedFilteredTruckList, pagedTruckList = List.take 100 newSortedFilteredTruckList, currentPageNumber = 1}

                -- uiModelUpdatedWithLatestSearchFilters =
                --         rebuildSearchFiltersBasedOnCurrentSearchCriteria model newUIModel

            in
                ( ( model , newUIModel), Cmd.none)--sendMessage ( FilterCheckBoxClicked 0 SalesStatus True ) )
                --( ( newModel , uiModelUpdatedWithLatestSearchFilters), Cmd.none)--sendMessage ( FilterCheckBoxClicked 0 SalesStatus True ) )

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
                apuFilters = buildSearchFilterValueRecordList APU uiModel.apuFilters trucks
                cdlFilters = buildSearchFilterValueRecordList CDL uiModel.apuFilters trucks
                photoFilters = buildSearchFilterValueRecordList Photo uiModel.apuFilters trucks

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
                                        owningBranchFilters = owningBranchFilters,
                                        apuFilters = apuFilters,
                                        cdlFilters = cdlFilters,
                                        photoFilters = photoFilters
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

        ShowAppraisedTrucks ->
            ( ( {model |
                            filteredTruckList = [],
                            truckList = [],
                            pagedTruckList = []} , {uiModel | searchString = ""}), fetchAppraisedTrucks "")


        ShowTrucksWithPhotoOnly ->
            ( (model, uiModel), Cmd.none )
            -- let
            --     photoOnlyTrucks =
            --         model.filteredTruckList
            --             |> List.filter (\t -> t.primaryImageLink /= "")
            --             |> (\list -> 
            --                         if List.length list > 0 then
            --                             list
            --                         else 
            --                             model.filteredTruckList)
                
            --     newModel = {model | truckList=photoOnlyTrucks,  filteredTruckList=photoOnlyTrucks, pagedTruckList = List.take 100 photoOnlyTrucks}

            --     uiModelUpdatedWithLatestSearchFilters =
            --              rebuildSearchFiltersBasedOnCurrentSearchCriteria newModel uiModel
            -- in        
            --     ( (newModel, uiModelUpdatedWithLatestSearchFilters), Cmd.none )

---- VIEW ----

textBox uiModel=

    Input.text [ wpx 300,   bw 0, pd 8
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
                            ([ bc 226 63 63, fc 250 250 250], image [hf,wf, bw one] {src = "srch_white.ico", description ="Logo" })
                        else
                            ([ bc 198 201 206, fc 245 245 245], image [hf, wf, bw one] {src = "srch_grey.ico", description ="Logo" })

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
                    row[wf,  hpx 50,  alpha  1.99, brc 97 97 97 , bw 0, pd 0
                     , htmlAttribute <|  style "z-index" "40", htmlAttribute <|  style "position" "fixed"
                    ]
                    [
                             column[bc 245 245 245, wpx 315, hf, bwb 1, brc 97 97 97, bw 0][
                                    image [hpx 32, bw one, centerY] {src = "https://az832863.vo.msecnd.net/~/media/images/components/pagelogos/mhclogo.png?_=-381616326&h=61", description ="Logo" }
                            ]
                            ,row[hf, pd 5, bc 245 245 245, spx 15, bw 0, wf]
                            [ 
                                row[bw 1]
                                [
                                    lazy textBox uiModel
                                    ,
                                    Input.button ( [pd 10, wpx 45, hpx 45, eId "submitSrch"] ++ searchStringBtnStyle)
                                        { 
                                            onPress = if String.length uiModel.searchString > 0 then Just SearchPressed else Nothing --Just SearchPressed 
                                            ,label = searchBtnIcon
                                        }
                                ]
                                ,column[  hpx 45, bw 0][
                                    Input.button ( [  eal, hf, pdl 0, fs 16, eId "clearSrch", bw 1, mouseOver [fc 217 98 69] , fc 0 0 0])
                                        { 
                                            onPress = Just ClearSearchStringResults
                                            ,label = el[pd 5] <| textValue "Clear Results"
                                        }
                                ]
                                ,column[  hpx 45, bw 0][
                                    Input.button ( [  eal, hf, pdl 0, fs 16, eId "clearSrch", bw 1, mouseOver [fc 217 98 69] , fc 0 0 0])
                                        { 
                                            onPress = Just ShowAppraisedTrucks
                                            ,label = el[pd 5] <| textValue "Show Appraised"
                                        }
                                ]
                                    
                            ]
                            -- ,column[pdl 25, bc 248 248 248, wf, hf, bwb 1, brc 97 97 97, fc 97 97 97][
                            --         column[ bwl 2, pdl 3, brc 255 94 94, centerY]
                            --             [el [fs 26 ] <| textValue "Suresh Yadali"
                            --             ,el [fs 18, pdt 15 ] <| textValue "Kansas City, MO"
                            --     ]
                            -- ]
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
                        row[hf,wf, pde 55 3 100 3]
                        [     
                            -- Search Filter Panel
                            column [wpx 300,  spy 15,  bc 215 215 215, eat, pdt 5] 
                            [
                                row[centerY, bw 1,  pde 0 5 0 5, spaceEvenly, wf, spx 3 ]
                                [
                                    Input.button ( [wf,   hf, pdl 0, fs 16, bw 1, mouseOver [fc 217 98 69] , fc 0 0 0])
                                    { 
                                        onPress = Just <| CollapseAllClicked True
                                        ,label = el[pd 5, bw 0, eacx] <| textValue "Expand All"
                                    }
                                    ,
                                    Input.button ( [ wf,   hf, pdl 0, fs 16, bw 1, mouseOver [fc 217 98 69] , fc 0 0 0])
                                    { 
                                        onPress = Just <| CollapseAllClicked False
                                        ,label = el[pd 5, eacx] <| textValue "Collapse All"
                                    }
                                    --  checkbox [fs 16, bw 1,  hf, ear] {
                                    --     onChange = CollapseAllClicked
                                    --     ,icon =  (\chkVal -> Element.none) -- buildCollapseAllImage
                                    --     , label = labelLeft [centerX, pd 5] (el [] <| textValue <| "Expand All" )
                                    --     , checked = uiModel.expandCollapseAllChecked
                                    -- }
                                    --,
                                    --centerX, centerY , brc 215 23 89, bw 2
                                    -- Input.radio
                                    --     [ padding 10
                                    --     , spacing 20
                                    --     ]
                                    --     { onChange = CollapseAllClicked
                                    --     , selected = True
                                    --     , label = Input.labelAbove (textValue "Lunch")
                                    --     , options =
                                    --         [ Input.option PriceHighToLow (textValue "asdf!")
                                               
                                    --         , Input.option PriceLowToHigh (textValue "Taco!")
                                    --         , Input.option YearNewToOld (textValue "Gyro")
                                    --         ]
                                    --     }
                                    --     ,
                                    -- checkbox [fs 16, bw 1,  hf, ear] {
                                    --     onChange = CollapseAllClicked
                                    --     ,icon =  (\chkVal -> Element.none) -- buildCollapseAllImage
                                    --     , label = labelLeft [centerX, pd 5] (el [] <| textValue <| "Collapse All" )
                                    --     , checked = uiModel.collapseAllChecked
                                    -- }
                                ]
                                ,column[wf, spy 5, bc 240 240 240, bw 0 ]
                                    <| (
                                        if List.length model.filteredTruckList == 0 then
                                            [loaderIconElement]
                                        else 
                                            List.map 
                                                (\filterType -> lazy3 buildSearchFilterValuesGroup filterType.filterName model uiModel) 
                                                allFilterTypesMasterListWithItsInitialState
                                    )
                            ]
                            
                            -- Trucks Search Result List Panel 
                            ,column[  wf,  bw 0 ,pdl 15,  eat]
                            [
                                row[wf, bwb 0, hf , pd 0,  bc 215 215 215, bw 0]
                                [ 
                                    column[wf, hpx 35, bw 0]
                                    [
                                        wrappedRow [wf,  bw 0, pd 6 , eat, spx 5, spy 5]
                                            -- using <| u can avoid parans around the below func and its params
                                            <| buildPageNumbersView  model.filteredTruckList model.currentPageNumber
                                    ]
                                    ,row[hf, bwl 1, pdb 3,  bc 215 215 215, wfp 2]
                                    [
                                        column[wf, hf]
                                        [
                                            --     row[wf]
                                            --     [   
                                            --             el [eat,ear, pdb 0, pdr 5,bw 0,  fc 219 108 98] <| textValue <| "Total trucks found : " ++ (String.fromInt <| (List.length model.filteredTruckList))   
                                            --     ]
                                            
                                            --  ,  
                                             row[bw 0,  wf, eab]
                                                [
                                                    column[bw 0, pdl 15, wpx 475]
                                                    [
                                                        --el [eal, pdb 0, pdr 5,bwb 1, fc 97 97 97, onClick (ShowTrucksWithPhotoOnly), pointer] <| textValue <| "Photos only "
                                                             el [eal, eacy, bw 0,  fc  190 5 30] <| textValue <| "Total trucks found : " ++ (String.fromInt <| (List.length model.filteredTruckList))   
                                                    ]
                                                    ,
                                                    column [ pdl 15,bw 0,  fc 97 97 97, wf
                                                        , below (showSortOptionsDialog uiModel.showDropdown uiModel.currentSortBy)
                                                    ][
                                                         Input.button [pdl 5, fb, fh, bwb 0 ]  
                                                            { 
                                                                onPress = Just <| OperateSortDialog <| not <| uiModel.showDropdown
                                                                ,label = el[bwb 1, fac] <| textValue <| "Sort by : " ++ convertSortByToDescription uiModel.currentSortBy
                                                            }
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
                                                                                    Array.toList uiModel.fleetCodeFilters,
                                                                                    Array.toList uiModel.truckStatusFilters,
                                                                                    Array.toList uiModel.specialFinancingFilters,
                                                                                    Array.toList uiModel.owningBranchFilters,
                                                                                    Array.toList uiModel.apuFilters,
                                                                                    Array.toList uiModel.cdlFilters,
                                                                                    Array.toList uiModel.photoFilters,
                                                                                    Array.toList uiModel.priceFilters,
                                                                                    Array.toList uiModel.engineHPFilters,
                                                                                    Array.toList uiModel.sleeperInchesFilters,
                                                                                    Array.toList uiModel.wheelBaseFilters,
                                                                                    Array.toList uiModel.mileageFilters,
                                                                                    Array.toList uiModel.frontAxleWeightFilters,
                                                                                    Array.toList uiModel.rearAxleWeightFilters,
                                                                                    Array.toList uiModel.inventoryAgeFilters
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
                           row[wpx 25, hpx 20]
                                    [
                                        Input.button ([
                                                        if currentPageNumber /= num then
                                                            mouseOver [ bc  0 0 0, fc 250 250 250  ]
                                                        else
                                                            mouseOver [ bc  175 175 175 ]
                                                        ,
                                                        pd 0, wf, hf,    fb ] ++ (searchStringBtnStyle num))
                                            { 
                                                onPress = Just (PageNumberClicked num )
                                                ,label =  el[eacx,eacy] <| textValue <| String.fromInt num
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