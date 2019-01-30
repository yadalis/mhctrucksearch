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
import Element.Border as Border exposing (..)
import TruckViews.Truck exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import BusinessFunctions.TruckFunctions exposing (..)
import BusinessFunctions.SearchFilterFunctions exposing (..)
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
import Task
import Process
import Time


---- INIT ----

type alias OnLoadSearchFilter =
    String

init : OnLoadSearchFilter -> ( (Model, UIModel) , Cmd Msg)
init jsflg =
    ( (initialModel,initalUIModel jsflg)
        , Cmd.batch [getFetchURL "used" "" False] -- initially loads used/non-appraised trucks
        --, Cmd.batch [fetchTrucks, fetchSearchFilterRanges] -- this executes all commands in async manner, it seems ?
    )

---- UPDATE ----
 
update : Msg -> (Model, UIModel) -> ( (Model, UIModel) , Cmd Msg  )
update msg (model, uiModel) =
    let
        getTruckCondition userAction = 
                        if userAction then
                            "new"
                        else
                            "used"
        
        buildTrucksHttpCmd =
                let
                    trucksHttpCmd = getFetchURL (getTruckCondition uiModel.workWithNewTrucks) uiModel.searchString uiModel.workWithAppraisedTrucks
                in
                    --( ( { model | filteredTruckList = [], truckList = [], pagedTruckList = [] } , uiModel), trucksHttpCmd)
                    ( (model , uiModel), trucksHttpCmd)
    in
        case msg of
            OnFetchSearchFilterRanges response ->
                let
                    --vxx = Debug.log "OnFetchSearchFilterRanges " [response]

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
                                                --allRangeFilterTypesMasterList -- make sure this list is up to date with all possible range filters, this is defined in model.elm
                                                (List.map (\rangeFltrMeta -> rangeFltrMeta.filterName) rangeSearchFiltersInitialExpandState)
                    
                    allRangeSearchFiltersWithCountsWithItsFilterType = 
                                List.map 
                                        (
                                            \(rangeFltrType, rangeFltrWithNoCountsList) ->  
                                                    (rangeFltrType, (buildSearchFilterValueRecordList RangeValue rangeFltrType (Array.fromList <| rangeFltrWithNoCountsList) model.truckList ))
                                        ) 
                                allRangeSearchFiltersWithNoCountsWithItsFilterType

                    
                    fetchRangeFiltersPoplulatedWithCounts fltrType = 
                                case    (find (\(rangeFltrType,filtrArray)  -> rangeFltrType == fltrType ) allRangeSearchFiltersWithCountsWithItsFilterType) of
                                    Just item -> Tuple.second item
                                    Nothing -> Array.empty

                    executeRangeFilterFunc rangeFilterMeta currentUIModel =
                        rangeFilterMeta.pushModifiedFilterListBackInToUIModel currentUIModel (fetchRangeFiltersPoplulatedWithCounts rangeFilterMeta.filterName)

                    newUIModel = 
                        List.foldl
                                executeRangeFilterFunc
                                uiModel
                                (List.filter (\fltrMeta -> fltrMeta.filterStyle == RangeValue) partialSearchFiltersMetadata)

                in
                    ( ( model , {newUIModel | selectedFilterBullets = [] }), Cmd.none)--sendMessage ( FilterCheckBoxClicked 0 SalesStatus True ) )

            OnFetchTrucks response ->
                let
                    --vx = Debug.log "OnFetchTrucks " [response]

                    trucks = case response of
                                Ok truckList ->
                                        truckList
                                            |> sortTruckList uiModel.currentSortBy
                                Err err ->
                                        []
                    
                    --vx = Debug.log "asdfasfasd" [trucks]

                    executeRegularFilterFunc regularFilterMeta currentUIModel =
                            regularFilterMeta.pushModifiedFilterListBackInToUIModel 
                                                        currentUIModel 
                                                        (buildSearchFilterValueRecordList SingleValue regularFilterMeta.filterName (regularFilterMeta.filters currentUIModel)  trucks)

                    (newModel, newUIModel, cmd) = 
                    
                        if List.length trucks > 0 then
                            (
                                {model  | truckList = trucks,  filteredTruckList = trucks, pagedTruckList = List.take defaultTrucksPerPage trucks },
                                List.foldl
                                        executeRegularFilterFunc
                                        uiModel
                                        (List.filter (\fltrMeta -> fltrMeta.filterStyle == SingleValue) partialSearchFiltersMetadata),
                                fetchSearchFilterRanges
                            )
                        else
                            (
                                model,  
                                uiModel,
                                Cmd.none
                            )

                    updatedUIModel =
                        if List.length trucks > 0 then
                            {newUIModel | hasWarningsToPresent = False, userWarningMessage = "" }
                        else
                            {newUIModel | hasWarningsToPresent = True, userWarningMessage = "No trucks found for the text " ++ uiModel.searchString }
                    
                    -- pagedTruckList = List.take defaultTrucksPerPage trucks

                    -- executeRegularFilterFunc regularFilterMeta currentUIModel =
                    --      regularFilterMeta.pushModifiedFilterListBackInToUIModel 
                    --                                 currentUIModel 
                    --                                 (buildSearchFilterValueRecordList SingleValue regularFilterMeta.filterName (regularFilterMeta.filters currentUIModel)  trucks)

                    -- newUIModel = 
                    --     List.foldl
                    --             executeRegularFilterFunc
                    --             uiModel
                    --             (List.filter (\fltrMeta -> fltrMeta.filterStyle == SingleValue) partialSearchFiltersMetadata)

                in
                    -- if List.length trucks > 0 then
                    --     ( 
                    --         (
                    --             {model     | truckList = trucks,  filteredTruckList = trucks, pagedTruckList = pagedTruckList},
                    --             { newUIModel | selectedFilterBullets = [] }
                    --         )
                    --         , fetchSearchFilterRanges   -- change this, otherwise it will bring all json based range filters data again and again, you should only rebuild the range filter counts
                    --                                     -- but not regenrate the filters completely
                    --     ) 
                    -- else
                         ( ( newModel , updatedUIModel), cmd)

            FilterCheckBoxClicked selectedSearchFilter userAction->
                let
                    convertMaybeInt intValue =
                         case intValue of 
                                Just val -> val
                                Nothing -> 0

                    updateUserSelectedSearchFilter : Array SearchFilterType -> (Array SearchFilterType -> UIModel) -> UIModel -- Anonymous funcs
                    updateUserSelectedSearchFilter  filterList pushModifiedFilterListBackInToUIModelFunc =
                        let
                            sfIndex =
                                Array.toList filterList
                                    |> findIndex (\sf -> String.trim sf.searchFilterKey == String.trim selectedSearchFilter.searchFilterKey && sf.filterCategory == selectedSearchFilter.filterCategory)
                                    |> convertMaybeInt
                        in
                            if sfIndex > 0 then
                                Array.toList filterList
                                        |> find (\sf -> String.trim sf.searchFilterKey == String.trim selectedSearchFilter.searchFilterKey  && sf.filterCategory == selectedSearchFilter.filterCategory)
                                        |> Maybe.map (\sf -> {sf | userAction = userAction})
                                        |> Maybe.map (\sf -> Array.set sfIndex sf filterList)
                                        |> Maybe.map pushModifiedFilterListBackInToUIModelFunc
                                        |> Maybe.withDefault uiModel
                            else
                                uiModel

                    newUIModel = 
                            partialSearchFiltersMetadata
                                |> find (\sfMeta -> sfMeta.filterName == selectedSearchFilter.filterCategory)
                                |> Maybe.map (
                                                \sfMeta -> ( 
                                                            (sfMeta.filters uiModel |> updateUserSelectedSearchFilter) (sfMeta.pushModifiedFilterListBackInToUIModel uiModel)
                                                )
                                            )
                                -- the below condition should never happen unless you misspell in metadata list in model.elm file
                                |> Maybe.withDefault uiModel

                    -- displayValue = 
                    --     if selectedSearchFilter.filterCategory == TruckType then 
                    --             if selectedSearchFilter.searchFilterKey == "I" then
                    --                     "Inventory"
                    --             else if selectedSearchFilter.searchFilterKey == "A" then 
                    --                     "Appraisal"
                    --             else
                    --                     "Purchase Order"
                    --     else
                    --         selectedSearchFilter.searchFilterExtraData

                    newUIModelUpdatedWithSearchFilterBullets = 
                                    {newUIModel |
                                                    selectedFilterBullets = 
                                                        if userAction then
                                                            -- do not insert if the combo sf exists
                                                            newUIModel.selectedFilterBullets
                                                                |> findIndex (\sf -> String.trim sf.searchFilterKey == String.trim selectedSearchFilter.searchFilterKey && sf.filterCategory == selectedSearchFilter.filterCategory)
                                                                |> convertMaybeInt
                                                                |> (\idx -> 
                                                                        let
                                                                            selectedFilterBullet =  SearchFilterType -- TODO change this variable name
                                                                                        selectedSearchFilter.index 
                                                                                        selectedSearchFilter.searchFilterKey 
                                                                                        selectedSearchFilter.searchFilterExtraData
                                                                                        userAction
                                                                                        0
                                                                                        selectedSearchFilter.filterCategory 
                                                                        in
                                                                        
                                                                            if idx > 0 then
                                                                                newUIModel.selectedFilterBullets 
                                                                            else
                                                                                selectedFilterBullet :: newUIModel.selectedFilterBullets
                                                                    )
                                                        else
                                                            newUIModel.selectedFilterBullets
                                                                |> find (\sf -> String.trim sf.searchFilterKey == String.trim selectedSearchFilter.searchFilterKey && sf.filterCategory == selectedSearchFilter.filterCategory)
                                                                |> Maybe.map (\sf -> remove sf  newUIModel.selectedFilterBullets)
                                                                |> Maybe.withDefault newUIModel.selectedFilterBullets
                                    }

                    newSortedFilteredTruckList = applySearchFilters model newUIModelUpdatedWithSearchFilterBullets
                                                    |> sortTruckList uiModel.currentSortBy

                    uiModelUpdatedWithLatestSearchFilters =
                            rebuildSearchFiltersBasedOnCurrentSearchCriteria model newUIModelUpdatedWithSearchFilterBullets
                in
                    ( ( {model | filteredTruckList = newSortedFilteredTruckList, pagedTruckList = List.take defaultTrucksPerPage newSortedFilteredTruckList, currentPageNumber = 1 } , uiModelUpdatedWithLatestSearchFilters), Task.perform (\_ -> NOoP) (setViewport 0 0))

            NOoP ->
                    ( ( model , uiModel), Cmd.none)

            SearchString searchString ->
                    ( ( model , {uiModel | searchString = searchString}), Cmd.none)

            SearchPressed ->
                buildTrucksHttpCmd
                
            HandleKeyboardEvent ->
                buildTrucksHttpCmd
            
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
                    grps = greedyGroupsOf defaultTrucksPerPage model.filteredTruckList
                    grpByPageNumber = List.drop (pageNumber - 1) grps

                    firstList = case List.head grpByPageNumber of
                                        Just page -> page
                                        Nothing -> []
                
                in
                    ( ( {model | pagedTruckList = firstList, currentPageNumber = pageNumber } , uiModel ), Cmd.none )

            OperateSortDialog show ->
                ( (model, {uiModel | showDropdown = show }), Cmd.none )
            
            CloseUserWarningsDialog userAction ->
                ( (model, {uiModel | hasWarningsToPresent = userAction }), Cmd.none )
            
            SortTrucks sortBy ->
                let
                    sortedFilteredTruckList = 
                        sortTruckList sortBy <| model.filteredTruckList 

                    newModel =
                        {model | filteredTruckList = sortedFilteredTruckList, pagedTruckList = List.take defaultTrucksPerPage sortedFilteredTruckList, currentPageNumber = 1 }

                in
                    ( (newModel, {uiModel | currentSortBy = sortBy}), Cmd.none )

            WorkWithAppraisedTrucks userAction ->
                            ( ( {model |
                                filteredTruckList = [],
                                truckList = [],
                                pagedTruckList = []} , {uiModel | searchString = "", workWithAppraisedTrucks = userAction, selectedFilterBullets = []}), 
                                                        getFetchURL (getTruckCondition uiModel.workWithNewTrucks) "" userAction
                                                    )
            
            WorkWithNewTrucks userAction ->
                            ( ( {model |
                                filteredTruckList = [],
                                truckList = [],
                                pagedTruckList = []} , {uiModel | searchString = "", workWithAppraisedTrucks = False, workWithNewTrucks = userAction, selectedFilterBullets = []}), 
                                                        getFetchURL (getTruckCondition userAction) "" False
                                                    )

            ShowLoader userAction ->
                 ( (model, {uiModel | showLoader = userAction }), 
 
                   Cmd.none
                )

            ClearAllFilters ->
                let
                    executeRegularAndRangeFilterFunc filterMeta updatableUIModel =
                        filterMeta.pushModifiedFilterListBackInToUIModel 
                            updatableUIModel 
                            (resetFilters (filterMeta.filters updatableUIModel))
    
                    newUIModelx = 
                        List.foldl
                                executeRegularAndRangeFilterFunc
                                uiModel
                                partialSearchFiltersMetadata

                    newUIModel = {newUIModelx | selectedFilterBullets = []}
                    
                    sortedTrkList = model.truckList |> sortTruckList uiModel.currentSortBy

                    newModel = 
                        {model |
                                filteredTruckList = sortedTrkList,
                                pagedTruckList =List.take defaultTrucksPerPage sortedTrkList
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
        ,placeholder = Just (Input.placeholder [fs 14] (el [centerY] <| textValue "Keyword truck Search"))
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
                        row[wpx 305, bw 0]
                        [
                            image [hpx 35] {src = "mhclogo.png", description ="Logo" }
                            ,
                            el[pdl 5, fs 18, eab] <| textValue "v1.0.0 - Live"
                        ]
                        ,row[wf]
                        [
                            row[wpx 350, bw 1, greyBorder 200, 
                                    below (
                                            if uiModel.hasWarningsToPresent then
                                                row[pde 5 15 5 0, bw 1, bc 255 153 153 ]
                                                [
                                                    el [pd 15] <| textValue uiModel.userWarningMessage
                                                    ,
                                                    row[ bw 1, greyBg 228  ][
                                                            Input.button ( [])
                                                                                { 
                                                                                    onPress = Just <| CloseUserWarningsDialog False
                                                                                    ,label = textValue " x "
                                                                                }
                                                    ]
                                                ]
                                            else
                                                none
                                    )]
                            [   
                                -- you need bw 0 here to remove the border around textbox to make it blend with search button
                                Input.text[bw 0, Element.htmlAttribute(ExtraHtmlEvents.onEnter HandleKeyboardEvent)
                                ]
                                {
                                    onChange = SearchString
                                    ,text  = uiModel.searchString
                                    ,label = labelLeft [] none
                                    ,placeholder = Just (Input.placeholder [fs 14] (el [eacy] <| textValue "Keyword truck Search"))
                                }
                                ,
                                Input.button ([hf] ++ searchStringBtnStyle)
                                { 
                                    onPress = if String.length uiModel.searchString > 0 then Just SearchPressed else Nothing --Just SearchPressed 
                                    ,label = el[pde 0 5 0 5] <| textValue "SEARCH"-- searchBtnIcon
                                }
                            ]
                            ,row[ear, spx 15, fs 18, pdl 15]
                            [
                                if uiModel.showLoader then
                                    image [hpx 18, bw one, wf, pdl 5, bwb 2, eat] {src = "loader.gif", description ="Logo" }
                                else
                                    none
                                ,
                                -- checkbox [] 
                                -- {
                                --     onChange = WorkWithNewTrucks
                                --     ,icon = buildWorkWithAppraisedTrucksToggleImage
                                --     , label = labelRight [] <| 
                                --                                 if  uiModel.workWithNewTrucks then 
                                --                                     textValue <| "Stop Browsing New Trucks"
                                --                                 else
                                --                                     textValue <| "Browse New Trucks"
                                --     , checked =
                                --                 uiModel.workWithNewTrucks
                                -- }
                                -- ,
                                row[bwl 1, hpx 30][]   
                                -- ,
                                -- if not uiModel.workWithNewTrucks then
                                --     checkbox [] 
                                --     {
                                --         onChange = WorkWithAppraisedTrucks
                                --         ,icon = buildWorkWithAppraisedTrucksToggleImage
                                --         , label = labelRight [] <| 
                                --                                     if  uiModel.workWithAppraisedTrucks then 
                                --                                         textValue <| "Stop Browsing Appraised Trucks"
                                --                                     else
                                --                                         textValue <| "Browse Appraised Trucks"
                                --         , checked =
                                --                     uiModel.workWithAppraisedTrucks
                                --     }
                                -- else
                                --     none
                                ,
                                if not uiModel.workWithNewTrucks then
                                    row[bwl 1, hpx 30][]
                                else
                                    none
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
                    ,row[wf, spx 15, greyBg 235,
                        Border.shadow  { offset = ( 0, 3 )
                                , size = 1
                                , blur = 15.0
                                , color = rgb255 185 185 185
                                }
                    , hfRange 40 65, clipY]
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
                                onPress =  if anyFilterApplied uiModel then Just <| ClearAllFilters  else Nothing ----  (ShowLoader True) 
                                ,label = el[  bwb 1] <| textValue "CLEAR FILTERS"
                            }
                        ]
                        ,
                        -- pager/totaltrucks-found
                        row[wf, spx 50, pdl 5]
                        [ 
                            el [mhcRed] <| textValue <| "Total " ++ (if uiModel.workWithNewTrucks  then "(NEW)"  else  "(USED)") ++  " trucks found : " ++ (String.fromInt <| (List.length model.filteredTruckList))   
                            ,
                            row[wfp 2] --clipY cuts the content of pager number if it goes beyond 65 height, this could happen
                            --if user resize the browser to a smaller width/height
                            [
                                wrappedRow [wf, pd 8 , spx 5, spy 5]
                                    -- using <| u can avoid parans around the below func and its params
                                    <| buildPageNumbersView  model.filteredTruckList model.currentPageNumber
                            ]
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
                        
                        row[wf, pde 90 3 0 3, spx 16 ]
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
                                                partialSearchFiltersMetadata
                                    )
                            ]
                            -- -- Trucks search Filter Bullets & Search Result List Panel 
                            ,
                            column[wf, hf]
                            [
                                lazy searchFilterBulletView 
                                        <| uiModel.selectedFilterBullets --Array.fromList <| concatAllFilters uiModel
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

subscriptions : (Model,UIModel) -> Sub Msg
subscriptions model =
    --Time.every 5000.00 (\pox -> SearchPressed)
    Sub.none

main : Program OnLoadSearchFilter (Model,UIModel) Msg
main =
    Browser.element
        { view = view
        , init = init -- to capture flags from JS
        , update = update
        --, subscriptions = always Sub.none
        ,subscriptions = subscriptions
        }