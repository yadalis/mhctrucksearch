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
    ( (initialModel,initalUIModel jsflg) , Cmd.batch [fetchTrucks " " " " 1] ) -- initially loads first 100 trucks with all possible search filters and counts

---- UPDATE ----
 
update : Msg -> (Model, UIModel) -> ( (Model, UIModel) , Cmd Msg  )
update msg (model, uiModel) =
    let      
        getTrucksHttpCmd newUIModel = 
                let
                    searchFilterString = (String.join "|" <| List.filter (\sb -> sb /= "" ) (formattedSelectedFilterBullets newUIModel) )

                    searchText = if newUIModel.searchString == "" then "" else newUIModel.searchString
                    trucksHttpCmd = 
                        if List.length (formattedSelectedFilterBullets newUIModel) > 0 then
                            fetchTrucks searchFilterString searchText 1
                        else
                            fetchTrucks "" searchText 1
                    
                    --vvrs = Debug.log "fetch truck url " [searchFilterString]
                in
                    trucksHttpCmd

        executeTextSearch =
                    ( (model , uiModel), getTrucksHttpCmd uiModel)
    in
        case msg of
            OnFetchTrucks response ->
                let
                    {pages, searchFilters, trucks, totalTrucksCount, cleanSearchFilterBullets} = 
                            case response of
                                    Ok rangeFltrs ->
                                            rangeFltrs

                                    Err err ->
                                            TruckData 0 [] [] 0 True-- use this to show errors on page

                    newModel = {model  | truckList = trucks, totalTrucksCount = totalTrucksCount }
                    --asdfasdf = Debug.log "Asdfasd" [trucks] 
 
                    updatedUIModel = {uiModel | allSearchFilters = searchFilters, selectedFilterBullets = if cleanSearchFilterBullets then [] else uiModel.selectedFilterBullets, showLoader = False}
                in
                    (( newModel , updatedUIModel), Cmd.none)
                
            FilterCheckBoxClicked selectedSearchFilter userAction ->
                let

                    updateUserSelectedSearchFilter : Array SearchFilterType -> UIModel
                    updateUserSelectedSearchFilter  filterList =
                            Array.toList filterList
                                |> find (\sf -> sf.searchFilterKey == selectedSearchFilter.searchFilterKey && sf.filterCategory == selectedSearchFilter.filterCategory )
                                |> Maybe.map (\sf -> {sf | userAction = userAction})
                                |> Maybe.map (\sf -> Array.set sf.index sf filterList)
                                |> Maybe.map (\fltrs -> {uiModel | allSearchFilters = Array.toList fltrs})
                                |> Maybe.withDefault uiModel

                    newUIModel = 
                            Array.fromList uiModel.allSearchFilters
                                    |> updateUserSelectedSearchFilter

                    newUIModelUpdatedWithSearchFilterBullets = 
                            {newUIModel | 
                                            showLoader = True,
                                            selectedFilterBullets = 
                                                                    if userAction then
                                                                        let
                                                                            selectedFilterBullet =  SearchFilterType
                                                                                        selectedSearchFilter.index 
                                                                                        selectedSearchFilter.searchFilterKey 
                                                                                        selectedSearchFilter.searchFilterExtraData
                                                                                        userAction
                                                                                        0
                                                                                        selectedSearchFilter.filterCategory 
                                                                        in
                                                                                selectedFilterBullet :: newUIModel.selectedFilterBullets
                                                                    else
                                                                        newUIModel.selectedFilterBullets
                                                                            |> find (\sf -> sf.searchFilterKey == selectedSearchFilter.searchFilterKey && sf.filterCategory == selectedSearchFilter.filterCategory )
                                                                            |> Maybe.map (\sf -> remove sf newUIModel.selectedFilterBullets)
                                                                            |> Maybe.withDefault newUIModel.selectedFilterBullets
                            }

                    newSortedFilteredTruckList = model.truckList
                in
                    ( ( {model | truckList = newSortedFilteredTruckList, currentPageNumber = 1 } , newUIModelUpdatedWithSearchFilterBullets), getTrucksHttpCmd newUIModelUpdatedWithSearchFilterBullets )

            -- ApplyFilters ->
            --     let
            --         formattedSelectedFilterBullets = 
            --             if List.length uiModel.selectedFilterBullets > 0 then
            --                 partialSearchFiltersMetadata
            --                         |> List.map 
            --                                     (
            --                                         \eachFilterType -> 
            --                                                 uiModel.selectedFilterBullets
            --                                                         |> List.filter (\selectedFilterBullet -> selectedFilterBullet.filterCategory == eachFilterType.filterName) 
            --                                                         |> List.map (\eachBullet -> eachBullet.searchFilterExtraData )
            --                                                         |> String.join ","
            --                                                         |> \finalStr -> 
            --                                                                         if String.length finalStr > 0 then 
            --                                                                             eachFilterType.filterCode ++ "=" ++ finalStr
            --                                                                         else
            --                                                                             ""
            --                                     )
            --             else
            --                 []
            --     in
            --         ( ( model , {uiModel | showLoader = True}), getTrucksHttpCmd )
                
            SearchString searchString ->
                    ( ( model , {uiModel | searchString = searchString}), Cmd.none)

            SearchPressed ->
                executeTextSearch
                
            HandleKeyboardEvent ->
                executeTextSearch
            
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
            
            CloseUserWarningsDialog userAction ->
                ( (model, {uiModel | hasWarningsToPresent = userAction }), Cmd.none )
            
            SortTrucks sortBy sortOrder ->
                let
                    newUIModel = {uiModel |  showLoader = True, currentSortBy = sortBy, currentSortOrder = sortOrder}
                    

                    -- sortedFilteredTruckList = 
                    --     sortTruckList sortBy <| model.filteredTruckList 

                    newModel =
                       {model | currentPageNumber = 1 }
                in
                    ( (newModel, newUIModel), getTrucksHttpCmd newUIModel )

            ShowLoader userAction ->
                 ( (model, {uiModel | showLoader = userAction }), 
 
                   Cmd.none
                )

            ClearAllFilters ->
                let
                    newUIModel = {uiModel |  showLoader = True, searchString = ""}

                    cmd = fetchTrucks "" "" 1
                in
                   ( ( model, newUIModel), cmd )

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
                    if List.length model.truckList > 0 then
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

            spinnerRow = 
                column[wf,  htmlAttribute <|  style "z-index" "45", htmlAttribute <|  style "position" "fixed", alpha  0.45, spy 0]
                [
                    row[wf, hpx 1750,  greyBg 25]
                    [
                        image [ eacx,eacy, bw 0, hf, pdt 150] {src = "red_loader.gif", description ="Logo" }   
                        
                    ]
                ]

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
                            el[pdl 5, fs 18, eab] <| textValue "v1.0.2"
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
                                row[bwl 0, hpx 30][]   
                                ,
                                
                                if not uiModel.workWithNewTrucks then
                                    row[bwl 0, hpx 30][]
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
                                onPress =   if (anyFilterApplied uiModel || uiModel.searchString /= "") then Just <| ClearAllFilters  else Nothing ----  (ShowLoader True) 
                                ,label = el[  bwb 1] <| textValue "CLEAR FILTERS"
                            }
                            ,
                            --  Input.button ( [ mouseOver [fc 217 98 69] ])
                            -- { 
                            --     onPress = Just <| ApplyFilters
                            --     ,label = el[  bwb 1] <| textValue  "APPLY FILTERS"
                            -- }
                            -- ,
                            row[wf, spx 50, bwl 1, fs 18, pdl 10 ]
                            [ 
                                --el [mhcRed] <| textValue <| "Total " ++ (if uiModel.workWithNewTrucks  then "(NEW)"  else  "(USED)") ++  " trucks found : " ++ (String.fromInt <| (List.length model.truckList))   
                                el [mhcRed] <| textValue <| "Total trucks found : " ++ (String.fromInt <| model.totalTrucksCount)   
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
                        
                        -- pager/totaltrucks-found
                        
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
                        -- row[][if uiModel.showLoader then image [ eacx,eacy, bw 2] {src = "loader.gif", description ="Logo" }   else none]
                        -- ,
                        row[wf, pde 100 3 0 3, spx 16]
                        [     
                            -- Search Filter Panel
                            column [wpx 300, eat] 
                            [
                                column[wf, spy 5, greyBg 240,  pd 10, hf
                                        ,inFront( if uiModel.showLoader then spinnerRow else none )]
                                    <| (
                                        if List.length model.truckList == 0 then
                                            [loaderIconElement]
                                        else 
                                        --{searchFilterDisplayText: String, searchFilterStates: List SearchFilterState  }
                                            List.map 
                                                (\filterType -> lazy2 buildSearchFilterValuesGroup {searchFilterDisplayText = filterType.displayText, searchFilterStates = (Array.filter (\sState -> sState.searchFilterCustomType == filterType.filterName  ) uiModel.expandCollapseSearchFilterStates) } (List.filter (\f -> f.filterCategory == filterType.filterName) uiModel.allSearchFilters ) ) 

                                               --(\filterType -> lazy3 buildSearchFilterValuesGroup filterType.filterName model uiModel)                                                
                                                partialSearchFiltersMetadata
                                    )
                            ]
                            -- Trucks search Filter Bullets & Search Result List Panel 
                            ,
                            column[wf, hf]
                            [
                                lazy searchFilterBulletView 
                                        <| uiModel.selectedFilterBullets --Array.fromList <| concatAllFilters uiModel
                                ,
                                column[wf]
                                [
                                        lazy trucksView model.truckList -- model.filteredTruckList
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