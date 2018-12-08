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
import Element.Lazy as Lazy exposing(..)
import TruckViews.SearchFilterBullet exposing (..)

---- INIT ----

type alias OnLoadSearchFilter =
    String

init : OnLoadSearchFilter -> ( (Model, UIModel) , Cmd Msg)
init jsflg =
    ( (initialModel,initalUIModel jsflg)
        , fetchTrucks
    )

---- UPDATE ----

performFinalSearch : Model -> String -> Model
performFinalSearch model userSearchString =
    let
        searchFilterValueList = split "=" userSearchString -- "a:THERMOKING", "md:t880", "y:2019" etc...
        searchFilterTypeCode =     
            case List.head <| searchFilterValueList of -- gives first element in the list
                Just val -> val
                Nothing -> ""
        searchFilterValue =
                -- case List.foldl (Just >> always) Nothing searchFilterValueList of  -- gives last element in the list -- 1st style
                --     Just val -> val
                --     Nothing -> ""
                case List.head <| List.reverse searchFilterValueList of -- gives last element in the list -- 2nd style
                    Just val -> val
                    Nothing -> ""

        --logsToBrowswerDevTools = Debug.log "searchValues -> " [searchFilterTypeCode,searchFilterValue]

        searchResultTruckList  = 
                if String.isEmpty userSearchString then
                    model.truckList
                        --|> List.take 100
                else if ( (not <| String.isEmpty searchFilterTypeCode) && String.isEmpty searchFilterValue) then
                    []
                else
                    model.truckList      
                        |> List.filter (\t ->     

                                case searchFilterTypeCode of
                                    "ss"    -> startsWith  (toUpper searchFilterValue) (toUpper t.salesStatus) 
                                    "y"     -> startsWith  ( searchFilterValue) ( t.year) 
                                    "m"     -> startsWith  (toUpper searchFilterValue) (toUpper t.make) 
                                    "md"     -> startsWith  (toUpper searchFilterValue) (toUpper t.model) 
                                    "sr"     -> startsWith  (toUpper searchFilterValue) (toUpper t.sleeperRoof) 
                                    "sb"     -> startsWith  (toUpper searchFilterValue) (toUpper t.sleeperBunk) 
                                    _       -> False -- invalid search string entered by the user
                            -- else
                            --     False
                        )
                    --|> List.take 100
                    |> List.sortBy .make
        
        finalSearchResultTruckList =
            if List.length searchResultTruckList > 0 then
                searchResultTruckList
            else
                model.filteredTruckList
        
        newModel = {model | filteredTruckList = finalSearchResultTruckList}
    in
        newModel

update : Msg -> (Model, UIModel) -> ( (Model, UIModel) , Cmd Msg  )
update msg (model, uiModel) =
    case msg of
        OnFetchTrucks response ->
            let
                trucks = case response of
                            Ok truckList ->
                                    truckList
                                        --|> List.take 100
                            Err err ->
                                    []

                --c = Debug.log "Updated year list by held salesstatus"  [trucks]--, newUIModel1.yearFilters]

                salesStatusFilters = buildSearchFilterValueRecordList SalesStatus trucks
                yearFilters = buildSearchFilterValueRecordList Year trucks
                makeFilters = buildSearchFilterValueRecordList Make trucks
                modelFilters = buildSearchFilterValueRecordList MakeModel trucks
                sleeperRoofFilters = buildSearchFilterValueRecordList SleeperRoof trucks
                sleeperBunkFilters = buildSearchFilterValueRecordList SleeperBunk trucks
            in
                ( 
                    (
                        {   model     | truckList = trucks, filteredTruckList = (List.filter (\t -> t.year == "2019" ) trucks)},
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
                    , Cmd.none
                )
        
        FilterCheckBoxClicked index searchFilterCustomType userAction ->
            let

                getItemFromArray arrayItems =
                    arrayItems
                        |> Array.get index
                        |> Maybe.map (\mf -> { mf | userAction = userAction} )
                        
                updateUserSelectedSearchFilter : Array SearchFilterType -> (Array SearchFilterType -> UIModel) -> UIModel -- Anonymous funcs
                updateUserSelectedSearchFilter  filterList pushModifiedFilterListBackInToUIModel =
                    filterList
                        |> Array.get index
                        |> Maybe.map (\mf -> { mf | userAction = userAction} )
                        --|> getItemFromArray
                        |> Maybe.map (\mf -> Array.set index mf filterList)
                        |> Maybe.map pushModifiedFilterListBackInToUIModel
                        |> Maybe.withDefault uiModel

                buildSelectedSearchFilterItems : Array SearchFilterType -> Array SearchFilterType -- Anonymous funcs
                buildSelectedSearchFilterItems  filterList =
                    let
                        selectedFilterItem =
                                 case (filterList
                                            |> getItemFromArray) of
                                                Just item -> item
                                                Nothing -> SearchFilterType 0 "" False 0
                                
                        newSelectedFilterItems = 
                                if selectedFilterItem.searchFilterKey == "" then
                                    uiModel.selectedFilterItems
                                else
                                    if selectedFilterItem.userAction then
                                        Array.push selectedFilterItem uiModel.selectedFilterItems
                                    else
                                        Array.filter (\item -> item.searchFilterKey /= selectedFilterItem.searchFilterKey) uiModel.selectedFilterItems
                    in
                        newSelectedFilterItems
                     
                
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
                                        (\mfArr -> {uiModel | salesStatusFilters = mfArr, selectedFilterItems = buildSelectedSearchFilterItems uiModel.salesStatusFilters})  -- 3rd style is also a partial applications style
                                   
                            -----------------------------------------------------------------------------------------------------------------
                        Year -> 
                            (uiModel.yearFilters 
                                    |> updateUserSelectedSearchFilter) 
                                                            (\mfArr -> {uiModel | yearFilters = mfArr, selectedFilterItems = buildSelectedSearchFilterItems uiModel.yearFilters})
                            --      |> (\filters -> updateUserSelectedSearchFilter filters (\mfArr -> {uiModel | yearFilters = mfArr}) )
                                
                        Make -> 
                            (uiModel.makeFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | makeFilters = mfArr, selectedFilterItems = buildSelectedSearchFilterItems uiModel.makeFilters})
                                --|> (\filters -> updateUserSelectedSearchFilter filters (\mfArr -> {uiModel | makeFilters = mfArr}) )

                        MakeModel -> 
                            (uiModel.modelFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | modelFilters = mfArr, selectedFilterItems = buildSelectedSearchFilterItems uiModel.modelFilters})
                                --|> (\filters -> updateUserSelectedSearchFilter filters (\mfArr -> {uiModel | modelFilters = mfArr}) )

                        SleeperRoof -> 
                            (uiModel.sleeperRoofFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | sleeperRoofFilters = mfArr, selectedFilterItems = buildSelectedSearchFilterItems uiModel.sleeperRoofFilters})
                                --|> (\filters -> updateUserSelectedSearchFilter filters (\mfArr -> {uiModel | sleeperRoofFilters = mfArr}) )    

                        SleeperBunk -> 
                            (uiModel.sleeperBunkFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | sleeperBunkFilters = mfArr, selectedFilterItems = buildSelectedSearchFilterItems uiModel.sleeperBunkFilters})
                                --|> (\filters -> updateUserSelectedSearchFilter filters (\mfArr -> {uiModel | sleeperBunkFilters = mfArr}) )    

                newFilteredTruckList = applySearchFilters model newUIModel

                uiModelUpdatedWithLatestSearchFilters = rebuildSearchFiltersBasedOnCurrentSearchCriteria model newUIModel
                
            in
                --( ( {model | filteredTruckList = newFilteredTruckList } , newUIModel), sendMessage SearchPressed )
                ( ( {model | filteredTruckList = newFilteredTruckList } , uiModelUpdatedWithLatestSearchFilters), Cmd.none )

        SearchString searchString ->
            -- let
            --     newModel = 
            --         if String.length searchString > 0 then
            --             model
            --         else
            --             {model | filteredTruckList = model.truckList }

            -- in
                --( ( model , {uiModel | searchString = searchString}), sendMessage SearchPressed )
            ( ( model , {uiModel | searchString = searchString}), Cmd.none)

        SearchPressed ->
            ( (performFinalSearch model uiModel.searchString, uiModel ), Cmd.none )
            
        HandleKeyboardEvent ->
            ( (performFinalSearch model uiModel.searchString, uiModel ), Cmd.none )
        
        -- CollapseClicked searchFilterType userAction ->
        --     let
        --         x = if searchFilterType == SalesStatus then              
        --                 ( ( model , {uiModel | expandCollapseSalesStatusChecked = userAction}), Cmd.none )
        --             else
        --                 ( ( model , {uiModel | expandCollapseYearChecked = userAction}), Cmd.none )   
        --     in
        --         x

        CollapseAllClicked userAction ->
            ( ( model , {uiModel | expandCollapseAllChecked = userAction}), Cmd.none )
 

        RemoveSearchFilterItemFromPinnedSearchFilters pinnedSearchFilterItem ->
             let
                    selectedPinnedItem = case List.head <| Array.toList (Array.filter (\item -> item.searchFilterKey == pinnedSearchFilterItem.searchFilterKey) uiModel.selectedFilterItems) of
                                                        Just val -> val
                                                        Nothing -> SearchFilterType -1 "" False 0
                    itemFromSalesStatusList =  case  List.head <| Array.toList (Array.filter (\item -> item.searchFilterKey == selectedPinnedItem.searchFilterKey) uiModel.salesStatusFilters) of
                                                        Just val -> val
                                                        Nothing -> SearchFilterType -1 "" False 0

                    updatedSalesStatusFilterList = 
                        uiModel.salesStatusFilters
                            |> Array.get itemFromSalesStatusList.index
                            |> Maybe.map (\mf -> { mf | userAction = False} )
                            --|> getItemFromArray
                            |> Maybe.map (\mf -> Array.set itemFromSalesStatusList.index mf uiModel.salesStatusFilters)
                            |> Maybe.withDefault uiModel.salesStatusFilters
                    
                    updatedSelectedFilterList =
                        uiModel.selectedFilterItems
                            |> Array.filter (\item -> item.searchFilterKey /= selectedPinnedItem.searchFilterKey)
             in
             
            
                ( ( model , {uiModel | salesStatusFilters = updatedSalesStatusFilterList, selectedFilterItems = updatedSelectedFilterList} ), sendMessage (FilterCheckBoxClicked itemFromSalesStatusList.index SalesStatus False)  )
             
---- VIEW ----

textBox uiModel=

    Input.text [wf, hf, bw 0
                --,Element.htmlAttribute ( on "keydown" (Decode.map HandleKeyboardEvent  decodeKeyboardEvent) )
                , Element.htmlAttribute(ExtraHtmlEvents.onEnter HandleKeyboardEvent)
            ]
    {
        onChange = SearchString
        ,text  = uiModel.searchString
        ,label = labelLeft [] none
        ,placeholder = Just (Input.placeholder [] (el [] <| textValue "Fluid truck Search"))

    }


view : (Model, UIModel) -> Html Msg
view (model, uiModel) =
        let
            searchStringBtnStyle = 
                        if String.length uiModel.searchString > 0 then 
                            [ bc 226 63 63, fc 250 250 250] 
                        else
                            [ bc 198 201 206, fc 245 245 245]

            loaderIconElement = 
                    if List.length model.filteredTruckList > 0 then
                        none
                    else
                        image [hpx 18, bw one, wf, pdl 5, bwb 2, alignTop] {src = "loader.gif", description ="Logo" }  

            searchBtnIcon =
                    if String.length uiModel.searchString > 0 then 
                        image [hpx 32, bw one] {src = "srch_white.ico", description ="Logo" }
                    else
                        image [hpx 32, bw one] {src = "srch_grey.ico", description ="Logo" }
                        
            buildCollapseAllImage userAction =
                if userAction == True then 
                    image [hpx 32, bw one] {src = "collapse.png", description ="Logo" }
                else 
                    image [hpx 32, bw one] {src = "expand.png", description ="Logo" }

            focusStyle : Element.Option
            focusStyle =
                Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
        
            navBar =
                    row[wf, hpx 75,  bc 200 200 200 , fc 250 250 250, alpha  0.95, bwb 2, brc 0 0 0]
                    [
                        image [hpx 32, bw one] {src = "https://az832863.vo.msecnd.net/~/media/images/components/pagelogos/mhclogo.png?_=-381616326&h=61", description ="Logo" }
                        -- ,
                        -- el [pdl 25, Element.alignRight] <| textValue "Fluid & Powerfull truck search platform, get the result with less than blink of an eye !!!"
                    ] 
        in
            
                layoutWith {options = [focusStyle]}  [ inFront navBar ] --  inFront navBar is to make menu bar fixed
                --  [ hf, inFront navBar ] use must put hf in the array to make the scrollbarY work, otherwise screen just exaands
                -- in mormal web style and user has to scroll up and down the page
                <|
                    row[hf,wf, pde 150 50 0 50, spx 50]
                    [
                        column [hf, wfmin 350,  spy 0,  bc 221 221 221] -- Search Filter Panel bc 225 225 225, 
                        [
                            row[wf, pd 10, bw 0, spaceEvenly]
                            [ 
                                -- Input.text [wf, hf, bw 0
                                --             --,Element.htmlAttribute ( on "keydown" (Decode.map HandleKeyboardEvent  decodeKeyboardEvent) )
                                --             , Element.htmlAttribute(ExtraHtmlEvents.onEnter HandleKeyboardEvent)
                                --         ]
                                -- {
                                --     onChange = SearchString
                                --     ,text  = uiModel.searchString
                                --     ,label = labelLeft [] none
                                --     ,placeholder = Just (Input.placeholder [] (el [] <| textValue "Fluid truck Search"))

                                -- }
                                lazy textBox uiModel
                                -- ,Input.text [wf, hf, bw 0
                                --             --,Element.htmlAttribute ( on "keydown" (Decode.map HandleKeyboardEvent  decodeKeyboardEvent) )
                                --             ,Element.htmlAttribute(ExtraHtmlEvents.onEnter HandleKeyboardEvent)
                                --         ]
                                -- {
                                --     onChange = SearchString
                                --     ,text  = uiModel.searchString
                                --     ,label = labelLeft [] none
                                --     ,placeholder = Just (Input.placeholder [] (el [] <| textValue "Fluid trucks Search"))
                                
                                -- }
                                ,Input.button ( [ hf, wpx 50, eId "submitSrch"] ++ searchStringBtnStyle)
                                    { 
                                        onPress = Just SearchPressed --if String.length uiModel.searchString > 0 then Just SearchPressed else Nothing
                                        ,label = searchBtnIcon
                                    }
                            ]
                            -- ,row[pd 5,   bc 245 245 245, wf, bw 0]
                            -- [
                            --     checkbox [bw one, hf, far , bw 0] {
                            --         onChange = CollapseAllClicked
                            --         ,icon = buildCollapseAllImage
                            --         , label = labelLeft [Element.alignRight] (el [] <| textValue <| if uiModel.expandCollapseAllChecked then "Collapse Filters" else "Expand Filters" )
                            --         , checked = uiModel.expandCollapseAllChecked
                            --     }
                            -- ]
                            ,column[scrollbarY,hf, wf, spy 20, pdt 15, bw 0,  bc 245 245 245 ]
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
                            ]
                        ]
                        
                        ,column[hf, wfp 5,  bwl 0 ,pde 0 0 0 0 ] -- Trucks Search Result List Panel 
                        [
                            row[hf, wf, bwb 1, hpx 65, pd 10,  bc 221 221 221]
                            [ 
                                --column[pdl 0, hf][] --, bc 244 66 95,
                                
                                -- column[hf,wf, bwr 2, pd 3][
                                --     --el [] << textValue <| "Selected Filters... ", 
                                --     --el [Element.alignBottom, pdr 5] <| textValue <| "Total trucks found : " ++ (String.fromInt <| (List.length model.filteredTruckList))
                                --     --el [Element.alignBottom, pdr 5] <| textValue <| "Total trucks found : " ++ (String.fromInt <| (List.length model.filteredTruckList))
                                --      lazy searchFilterBulletView uiModel.modelFilters

                                -- ],
                                column[hf,Element.alignRight, bwb 0, pd 3][
                                    --el [] << textValue <| "Selected Filters... ", 
                                    el [Element.alignBottom, pdr 5] <| textValue <| "Total trucks found : " ++ (String.fromInt <| (List.length model.filteredTruckList))
                                    --el [Element.alignBottom, pdr 5] <| textValue <| "Total trucks found : " ++ (String.fromInt <| (List.length model.filteredTruckList))
                                ]
                            ]
                            --,column[hf, wf, scrollbarY, bw 0, pde 10 10 10 0] [ lazy trucksView model.filteredTruckList]
                            ,row[ wf, bwb 0, pdt 5][
                                row[  wf,  bw 0, pd 0 ][
                                    lazy searchFilterBulletView uiModel.selectedFilterItems
                                ]
                                
                                -- ,
                                 
                                --     lazy trucksView model.filteredTruckList
                                
                            ]
                            -- ,row[ wf, bwb 2, pdb 3][
                            --     row[  wf,  bw 0, pd 0 ][
                            --         lazy searchFilterBulletView uiModel.yearFilters
                                    
                            --     ]
                                
                            --     -- ,
                                 
                            --     --     lazy trucksView model.filteredTruckList
                                
                            -- ]
                            ,column[ scrollbarY, wf,  bw 0, pde 15 5 5 5 ][
                                    lazy trucksView model.filteredTruckList
                                ]
                            
                            -- ,row[hf, wf, bw 0, hpx 50, pde 10 10 10 10]
                            -- [ 
                            --     column[pdl 15, hf][] --, bc 244 66 95
                            --     ,column[hf, pdl 5, spaceEvenly][
                            --         -- el [] <| textValue <| "Page nav bar... ", 
                            --         -- el [] <| textValue <| "Total Used Trucks : " ++ (String.fromInt <| (List.length model.truckList))
                            --     ]
                            -- ]                  
                        ]
                        -- ,column[bw 0, wfp 4, hf, pd 0]
                        -- [
                        --     row[bc 200 200 200, wf, hf, scrollbarY]
                        --     [
                        --         el [alignTop] <| textValue <| ""
                        --     ]
                            
                        -- ]
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
