module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Commands exposing (..)
--import RemoteData  exposing (..)
import Http exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Element exposing (..)
import Element.Input as Input exposing (..) 
import Element.Border as Border exposing (..) 
import Element.Font as Font exposing (..) 
import TruckViews.Truck exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Helpers.Utils exposing (..)
import BusinessFunctions.TruckFunctions exposing (..)
import Task
import Array exposing(..)
import String exposing (..)
import Html.Events.Extra as ExtraHtmlEvents
import SearchFilterViews.SearchFilter exposing (..)
--import SearchFilterViews.SearchFilterRage exposing (..)
import Element.Lazy as Lazy exposing(..)
import TruckViews.SearchFilterBullet exposing (..)
import List.Extra exposing (..)
import TruckViews.SortDialog exposing (..)
import List.Unique exposing (..)

---- INIT ----

type alias OnLoadSearchFilter =
    String

init : OnLoadSearchFilter -> ( (Model, UIModel) , Cmd Msg)
init jsflg =
    ( (initialModel,initalUIModel jsflg)
        , Cmd.batch [fetchTrucks]
        --, Cmd.batch [fetchTrucks, fetchSearchFilterRanges] -- this executes all commands in async manner, it seems ?
    )

---- UPDATE ----

performFinalSearch : Model -> String -> UIModel -> (Model, UIModel)
performFinalSearch model userSearchString uiModel =
    let
        searchResultTruckList  = 
                    model.truckList      
                        |> List.filter (\t ->     
                            startsWith  (toLower userSearchString) (toLower t.salesStatus) ||
                            startsWith  (toLower userSearchString) (toLower t.year) ||
                            startsWith  (toLower userSearchString) (toLower t.make) ||
                            startsWith  (toLower userSearchString) (toLower t.model) ||
                            startsWith  (toLower userSearchString) (toLower t.sleeperRoof)||
                            startsWith  (toLower userSearchString) (toLower t.sleeperBunk)
                        )
                    |> sortTruckList uiModel.currentSortBy
        
        (finalSearchResultTruckList, hasTextSearchReturnedAnyResult) =
            if List.length searchResultTruckList > 0 then
                (searchResultTruckList, True)
            else
                (model.filteredTruckList, False)

        ggg = Debug.log "222222222222222222222" hasTextSearchReturnedAnyResult
        

        newModel = {model | filteredTruckList = finalSearchResultTruckList, pagedTruckList = List.take 100 finalSearchResultTruckList}

        uiModelUpdatedWithLatestSearchFilters = rebuildSearchFiltersBasedOnTextSeachResults newModel {uiModel | hasTextSearchReturnedAnyResult = hasTextSearchReturnedAnyResult}

        asd=  Debug.log " hasTextSearchReturnedAnyResult " [uiModelUpdatedWithLatestSearchFilters.hasTextSearchReturnedAnyResult]
    in
        (newModel, uiModelUpdatedWithLatestSearchFilters)

update : Msg -> (Model, UIModel) -> ( (Model, UIModel) , Cmd Msg  )
update msg (model, uiModel) =
    case msg of
        OnFetchSearchFilterRanges response ->
            let
                --x =  Debug.log "ranges" response

                priceFiltersList = case response of
                                    Ok priceFilterList ->
                                            priceFilterList
                                                --|> List.take 100
                                    Err err ->
                                            []

                --x =  Debug.log "ranges before" priceFiltersList
                --priceFilters = buildSearchFilterValueRangeList Price (Array.fromList <| priceFiltersList) model.truckList
                priceFilters = buildSearchFilterValueRecordList Price (Array.fromList <| priceFiltersList) model.truckList
                --x1 =  Debug.log "ranges after " priceFilters
                --y = Debug.log "asdfasdfasdfsadfasddsaf" [List.map .price model.truckList]
                
            in
            
                ( ( model , {uiModel | priceFilters = priceFilters} ), Cmd.none)
                --( ( model , uiModel ), Cmd.none)

        OnFetchTrucks response ->
            let
                trucks = case response of
                            Ok truckList ->
                                    truckList
                                        --|> List.take 100
                            Err err ->
                                    []

                -- x = List.map .sleeperInches trucks

                -- u = Debug.log "asddddddddddddddd" [x]
                
                --c = Debug.log "Updated year list by held salesstatus"  [trucks]--, newUIModel1.yearFilters]

                salesStatusFilters = buildSearchFilterValueRecordList SalesStatus uiModel.salesStatusFilters trucks
                yearFilters = buildSearchFilterValueRecordList Year uiModel.yearFilters trucks
                makeFilters = buildSearchFilterValueRecordList Make uiModel.makeFilters trucks
                modelFilters = buildSearchFilterValueRecordList MakeModel uiModel.modelFilters trucks
                sleeperRoofFilters = buildSearchFilterValueRecordList SleeperRoof uiModel.sleeperRoofFilters trucks
                sleeperBunkFilters = buildSearchFilterValueRecordList SleeperBunk uiModel.sleeperBunkFilters trucks

                --filteredTruckList = List.filter (\t -> t.year == "2019" ) trucks
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
                                        sleeperBunkFilters = sleeperBunkFilters 
                        }
                    )
                    --, Cmd.none
                    , fetchSearchFilterRanges
                )
        -- FilterRangeCheckBoxClicked index searchFilterRangeUnionType userAction  ->
        --      let
        --         updateUserSelectedSearchRangeFilter : Array SearchFilterRangeType -> (Array SearchFilterRangeType -> UIModel) -> UIModel -- Anonymous funcs
        --         updateUserSelectedSearchRangeFilter  filterList pushModifiedFilterListBackInToUIModel =
        --             filterList
        --                 |> Array.get index
        --                 |> Maybe.map (\mf -> { mf | userAction = userAction} )
        --                 |> Maybe.map (\mf -> Array.set index mf filterList)
        --                 |> Maybe.map pushModifiedFilterListBackInToUIModel
        --                 |> Maybe.withDefault uiModel

        --         newUIModel = 
        --             case searchFilterRangeUnionType of
                        
        --                 Price -> 
        --                     (uiModel.priceFilters |> updateUserSelectedSearchRangeFilter) (\mfArr -> {uiModel | priceFilters = mfArr})
        --                         --|> (\filters -> updateUserSelectedSearchFilter filters (\mfArr -> {uiModel | sleeperBunkFilters = mfArr}) )    

        --         newFilteredTruckList = applySearchFilters model newUIModel

        --         uiModelUpdatedWithLatestSearchFilters = rebuildSearchFiltersBasedOnCurrentSearchCriteria model newUIModel

        --         pagedTruckList = List.take 100 newFilteredTruckList
                
                
        --     in
        --         --( ( {model | filteredTruckList = newFilteredTruckList } , newUIModel), sendMessage SearchPressed )
        --         ( ( {model | filteredTruckList = newFilteredTruckList, pagedTruckList = pagedTruckList, currentPageNumber = 1 } , uiModelUpdatedWithLatestSearchFilters), Cmd.none )


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
                            
                            --|> (\filters -> updateUserSelectedSearchFilter filters (\mfArr -> {uiModel | salesStatusFilters = mfArr}) ) -- first style
                            ----------------------------------------------------------------------------------------------------------------
                            -- let
                            --     fx = uiModel.salesStatusFilters
                            --             |> updateUserSelectedSearchFilter
                                
                            -- in
                            --     fx  (\mfArr -> {uiModel | salesStatusFilters = mfArr}) -- second style is a partial applications style
                            -----------------------------------------------------------------------------------------------------------------
                             
                            (updateUserSelectedSearchFilter <| uiModel.salesStatusFilters)
                                        (\mfArr -> {uiModel | salesStatusFilters = mfArr})  -- 3rd style is also a partial applications style
                                   
                            -----------------------------------------------------------------------------------------------------------------
                        Year -> 
                            (uiModel.yearFilters 
                                    |> updateUserSelectedSearchFilter) 
                                                            (\mfArr -> {uiModel | yearFilters = mfArr})
                            --      |> (\filters -> updateUserSelectedSearchFilter filters (\mfArr -> {uiModel | yearFilters = mfArr}) )
                                
                        Make -> 
                            (uiModel.makeFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | makeFilters = mfArr})
                                --|> (\filters -> updateUserSelectedSearchFilter filters (\mfArr -> {uiModel | makeFilters = mfArr}) )

                        MakeModel -> 
                            (uiModel.modelFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | modelFilters = mfArr})
                                --|> (\filters -> updateUserSelectedSearchFilter filters (\mfArr -> {uiModel | modelFilters = mfArr}) )

                        SleeperRoof -> 
                            (uiModel.sleeperRoofFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | sleeperRoofFilters = mfArr})
                                --|> (\filters -> updateUserSelectedSearchFilter filters (\mfArr -> {uiModel | sleeperRoofFilters = mfArr}) )    

                        SleeperBunk -> 
                            (uiModel.sleeperBunkFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | sleeperBunkFilters = mfArr})
                                --|> (\filters -> updateUserSelectedSearchFilter filters (\mfArr -> {uiModel | sleeperBunkFilters = mfArr}) )    

                        Price -> 
                            (uiModel.priceFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | priceFilters = mfArr})
                                --|> (\filters -> updateUserSelectedSearchFilter filters (\mfArr -> {uiModel | sleeperBunkFilters = mfArr}) )    

                newFilteredTruckList = applySearchFilters model newUIModel
                                            |> sortTruckList uiModel.currentSortBy

                
                uiModelUpdatedWithLatestSearchFilters =
                    if uiModel.hasTextSearchReturnedAnyResult then
                        rebuildSearchFiltersBasedOnTextSeachResults model newUIModel
                    else
                        rebuildSearchFiltersBasedOnCurrentSearchCriteria model newUIModel

                pagedTruckList = List.take 100 newFilteredTruckList
                
                
            in
                --( ( {model | filteredTruckList = newFilteredTruckList } , newUIModel), sendMessage SearchPressed )
                ( ( {model | filteredTruckList = newFilteredTruckList, pagedTruckList = pagedTruckList, currentPageNumber = 1 } , uiModelUpdatedWithLatestSearchFilters), Cmd.none )

        ClearSearchStringResults ->
                --( ( {model | filteredTruckList = model.truckList} , {uiModel | searchString = ""}), Cmd.none)
            let
                 
                trucks = model.truckList
                salesStatusFilters = buildSearchFilterValueRecordList SalesStatus uiModel.salesStatusFilters trucks
                yearFilters = buildSearchFilterValueRecordList Year uiModel.yearFilters trucks
                makeFilters = buildSearchFilterValueRecordList Make uiModel.makeFilters trucks
                modelFilters = buildSearchFilterValueRecordList MakeModel uiModel.modelFilters trucks
                sleeperRoofFilters = buildSearchFilterValueRecordList SleeperRoof uiModel.sleeperRoofFilters trucks
                sleeperBunkFilters = buildSearchFilterValueRecordList SleeperBunk uiModel.sleeperBunkFilters trucks

                --filteredTruckList = List.filter (\t -> t.year == "2019" ) trucks
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
                                        sleeperBunkFilters = sleeperBunkFilters 
                        }
                    )
                    --, Cmd.none
                    , fetchSearchFilterRanges
                )

        SearchString searchString ->
                ( ( model , {uiModel | searchString = searchString}), Cmd.none)

        SearchPressed ->
            (
                let
                    result =  performFinalSearch model uiModel.searchString uiModel
                    newModel = Tuple.first result    
                    newUIModel = Tuple.second result
                in
                    (newModel, newUIModel ), Cmd.none 
            )
            
        HandleKeyboardEvent ->
            (
                let
                    result =  performFinalSearch model uiModel.searchString uiModel
                    newModel = Tuple.first result    
                    newUIModel = Tuple.second result
                in
                    (newModel, newUIModel ), Cmd.none 
            )
        
        CollapseClicked searchFilterState userAction->
            let
                newSearchFilterState = {searchFilterState | userAction = userAction }
                updatedSearchFilterStates = 
                    uiModel.expandCollapseSearchFilterStates
                        |> Array.set searchFilterState.index newSearchFilterState
            in
                ( ( model , {uiModel |  expandCollapseSearchFilterStates = updatedSearchFilterStates}), Cmd.none )

        -- CollapseRangeClicked searchFilterRangeState userAction->
        --     let
        --         newSearchFilterRangeState = {searchFilterRangeState | userAction = userAction }
        --         updatedSearchFilterRangeStates = 
        --             uiModel.expandCollapseSearchFilterRangeStates
        --                 |> Array.set searchFilterRangeState.index newSearchFilterRangeState
        --     in
        --         ( ( model , {uiModel |  expandCollapseSearchFilterRangeStates = updatedSearchFilterRangeStates}), Cmd.none )


        CollapseAllClicked userAction ->
            let
                updatedSearchFilterStates = 
                    uiModel.expandCollapseSearchFilterStates
                        |> Array.map (\item -> {item | userAction = userAction})
                
                -- updatedSearchFilterRangeStates = 
                --     uiModel.expandCollapseSearchFilterRangeStates
                --         |> Array.map (\item -> {item | userAction = userAction})
            in
                ( ( model , {uiModel |  expandCollapseSearchFilterStates = updatedSearchFilterStates, 
                                        --expandCollapseSearchFilterRangeStates = updatedSearchFilterRangeStates,
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


                hasAnyFilterApplied = anySearchFilterBulletsApplied 
                                                << Array.fromList <| List.concat
                                                                                [ 
                                                                                    Array.toList uiModel.salesStatusFilters,
                                                                                    Array.toList uiModel.yearFilters,
                                                                                    Array.toList uiModel.makeFilters,
                                                                                    Array.toList uiModel.modelFilters,
                                                                                    Array.toList uiModel.sleeperRoofFilters,
                                                                                    Array.toList uiModel.sleeperBunkFilters,
                                                                                    Array.toList uiModel.priceFilters
                                                                                ]
                sortedFilteredTruckList = 
                    sortTruckList sortBy <| model.filteredTruckList 

                newModel =
                    {model | filteredTruckList = sortedFilteredTruckList, pagedTruckList = List.take 100 sortedFilteredTruckList, currentPageNumber = 1 }

            in
                ( (newModel, {uiModel | currentSortBy = sortBy}), Cmd.none )

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
        ,placeholder = Just (Input.placeholder [Font.size 14] (el [centerY] <| textValue "Fluid truck Search"))

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
                        image [hpx 18, bw one, wf, pdl 5, bwb 2, alignTop] {src = "loader.gif", description ="Logo" }  

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
                                        [el [Font.size 26, letterSpacing 0 ] <| textValue "Suresh Yadali"
                                        ,el [Font.size 18, pdt 15, letterSpacing 0] <| textValue "Kansas City, MO"
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
                            column [wf,  spy 15,  bc 215 215 215, alignTop] 
                            [
                                row[wf, pd 10, bw 0]
                                [ 
                                    lazy textBox uiModel
                                    ,Input.button ( [hf, wpx 50, eId "submitSrch"] ++ searchStringBtnStyle)
                                        { 
                                            onPress = if String.length uiModel.searchString > 0 then Just SearchPressed else Nothing --Just SearchPressed 
                                            ,label = searchBtnIcon
                                        }
                                ]
                                ,row[centerY, bw 0, wf ]
                                [
                                    Input.button ( [hf, pd 3, Font.size 18, eId "clearSrch", bw 2])
                                        { 
                                            onPress = Just ClearSearchStringResults
                                            ,label = textValue "Clear Results"
                                        }
                                    ,
                                    checkbox [ pdr 5] {
                                        onChange = CollapseAllClicked
                                        ,icon = buildCollapseAllImage
                                        , label = labelLeft [Element.alignRight] (el [] <| textValue <| if uiModel.expandCollapseAllChecked then "Collapse All" else "Expand All" )
                                        , checked = uiModel.expandCollapseAllChecked
                                    }
                                ]
                                ,column[wf, spy 5, bc 240 240 240, bw 0 ]
                                [
                                    if List.length model.filteredTruckList > 0 then
                                        lazy3 buildSearchFilterValuesGroup SalesStatus model uiModel
                                    else
                                        loaderIconElement
                                    ,if List.length model.filteredTruckList > 0 then
                                        lazy3 buildSearchFilterValuesGroup Year model uiModel
                                    else
                                        none
                                    ,if List.length model.filteredTruckList > 0 then
                                        lazy3 buildSearchFilterValuesGroup Make model uiModel
                                    else
                                        none    
                                    , if List.length model.filteredTruckList > 0 then
                                        lazy3 buildSearchFilterValuesGroup MakeModel model uiModel
                                    else
                                        none
                                    , if List.length model.filteredTruckList > 0 then
                                        lazy3 buildSearchFilterValuesGroup SleeperRoof model uiModel
                                    else
                                        none
                                    , if List.length model.filteredTruckList > 0 then
                                        lazy3 buildSearchFilterValuesGroup SleeperBunk model uiModel
                                    else
                                        none
                                    , if List.length model.filteredTruckList > 0 then
                                        lazy3 buildSearchFilterValuesGroup Price model uiModel
                                    else
                                        none                                                        
                                ]
                            ]
                            
                            -- Trucks Search Result List Panel 
                            ,column[  wfp 5,  bw 0 ,pdl 15,  alignTop]
                            [
                                row[wf, bwb 0, hfRange 65 150 , pd 0,  bc 215 215 215, bw 0]
                                [ 
                                    column[wfp 3, hf, bw 0]
                                    [
                                        wrappedRow [wf,  bw 0, pdl 5 , alignTop]
                                            -- using <| u can avoid parans around the below func and its params
                                            <| buildPageNumbersView  model.filteredTruckList model.currentPageNumber
                                    ]
                                    ,row[hf, bw 0, pdb 3,  bc 215 215 215,wfp 2]
                                    [
                                        column[bw 0, alignBottom, Element.alignLeft]
                                        [
                                            el [ pdr 15,bw 0, Element.alignLeft, fc 97 97 97, Element.alignBottom
                                                , below (showSortOptionsDialog uiModel.showDropdown)
                                            ]
                                            <| Input.button [pdl 5, wf,    Font.bold, Font.hairline, bwb 1  ]  
                                                { 
                                                    onPress = Just <| OperateSortDialog <| not <| uiModel.showDropdown
                                                    ,label = textValue <| "Sort by : " ++ convertSortByToString uiModel.currentSortBy
                                                }
                                        ]
                                        ,column[bw 0, alignBottom, Element.alignRight]
                                        [
                                            el [Element.alignBottom,pdb 5, pdr 5,bw 0, Element.alignRight, fc 97 97 97] <| textValue <| "Total trucks found : " ++ (String.fromInt <| (List.length model.filteredTruckList))
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
                                                                                    Array.toList uiModel.priceFilters
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
                        [  bwb 0, bc 185 185 185, fc 57 57 57 , Font.size 16]
                    else
                        [   bwb 0, fc 244 66 95  , Font.size 12]
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
                                                        pd 5, wf,    Font.bold ] ++ (searchStringBtnStyle num))
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
