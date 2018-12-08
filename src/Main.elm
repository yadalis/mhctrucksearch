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
import List.Extra exposing (..)

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

                filteredTruckList = List.filter (\t -> t.year == "2019" ) trucks
                pagedTruckList = List.take 100 filteredTruckList
            in
                ( 
                    (
                        {   model     | truckList = trucks,  filteredTruckList = filteredTruckList, pagedTruckList = pagedTruckList},
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

                newFilteredTruckList = applySearchFilters model newUIModel

                uiModelUpdatedWithLatestSearchFilters = rebuildSearchFiltersBasedOnCurrentSearchCriteria model newUIModel

                pagedTruckList = List.take 100 newFilteredTruckList
                
                
            in
                --( ( {model | filteredTruckList = newFilteredTruckList } , newUIModel), sendMessage SearchPressed )
                ( ( {model | filteredTruckList = newFilteredTruckList, pagedTruckList = pagedTruckList } , uiModelUpdatedWithLatestSearchFilters), Cmd.none )

        SearchString searchString ->
                ( ( model , {uiModel | searchString = searchString}), Cmd.none)

        SearchPressed ->
            ( (performFinalSearch model uiModel.searchString, uiModel ), Cmd.none )
            
        HandleKeyboardEvent ->
            ( (performFinalSearch model uiModel.searchString, uiModel ), Cmd.none )
        
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
                ( ( model , {uiModel |  expandCollapseSearchFilterStates = updatedSearchFilterStates, expandCollapseAllChecked = userAction}), Cmd.none )

        PageNumberClicked pageNumber totalPages ->
            let
                -- totalSearchResultTrucks = List.length model.filteredTruckList
                -- endPosition =   if (startPosition + 99) <= totalSearchResultTrucks then 
                --                     (startPosition + 99) 
                --                 else
                --                     ( totalSearchResultTrucks - startPosition )
                
                grps = greedyGroupsOf 100 model.filteredTruckList
                newgrps = List.drop (pageNumber - 1) grps

                firstList = case List.head newgrps of
                                    Just lst -> lst
                                    Nothing -> []
                
                
                --a = Debug.log "start & end" [startPosition, endPosition, totalSearchResultTrucks]
                a1 = Debug.log "start & end" [List.length <| case (List.head <| grps) of 
                                                                        Just val -> val
                                                                        Nothing -> []]
                
                a2 = Debug.log "grps leng" [List.length grps, totalPages]
                                                                 
            in
                ( ( {model | pagedTruckList = firstList } , uiModel ), Cmd.none )

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
                    row[ wfmax 1920, hpx 75,  fc 250 250 250, alpha  0.99]
                    [
                        column[wpx 50][]
                        ,column[bc 250 250 250, wfp 2, hf, bwb 2, brc 97 97 97][
                            image [hpx 32, bw one, centerY] {src = "https://az832863.vo.msecnd.net/~/media/images/components/pagelogos/mhclogo.png?_=-381616326&h=61", description ="Logo" }
                        ]
                        ,column[bc 240 240 240, wf, hf, bwb 2, brc 97 97 97][
                            
                        ]
                        ,column[wpx 50][]
                    ] 
        in
            
                layoutWith {options = [focusStyle]}  [pde 78 50 50 50, inFront navBar ] --  inFront navBar is to make menu bar fixed
                --  [ hf, inFront navBar ] use must put hf in the array to make the scrollbarY work, otherwise screen just exaands
                -- in mormal web style and user has to scroll up and down the page
                <|
                    --row[hf,wf, spx 25, wfmax 1920]
                    row[hf,wf, wfmax 1920]
                    [
                        -- Search Filter Panel
                        column [hf, wfmin 300,  spy 0,  bc 221 221 221] 
                        [
                            row[wf, pd 10, bwb 1, spaceEvenly]
                            [ 
                                lazy textBox uiModel
                                ,Input.button ( [ hf, wpx 50, eId "submitSrch"] ++ searchStringBtnStyle)
                                    { 
                                        onPress = if String.length uiModel.searchString > 0 then Just SearchPressed else Nothing --Just SearchPressed 
                                        ,label = searchBtnIcon
                                    }
                            ]
                            ,row[centerY,   bc 245 245 245, wf, bw 0, pdt 10]
                            [
                                checkbox [bw one, hf, far , bw 0, wf, pdr 5] {
                                    onChange = CollapseAllClicked
                                    ,icon = buildCollapseAllImage
                                    , label = labelLeft [Element.alignRight] (el [] <| textValue <| if uiModel.expandCollapseAllChecked then "Collapse All" else "Expand All" )
                                    , checked = uiModel.expandCollapseAllChecked
                                }
                            ]
                            ,column[scrollbarY,hf, wf, spy 5, pdt 15, bw 0,  bc 240 240 240 ]
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
                        
                         -- Trucks Search Result List Panel 
                        ,column[hf, wfp 5,  bwl 0 , pdl 25]
                        [
                            row[hf, wf, bwb 1, hpx 65, pd 10,  bc 221 221 221]
                            [ 
                                column[hf,Element.alignRight, bwb 0, pd 3][
                                    el [Element.alignBottom, pdr 5] <| textValue <| "Total trucks found : " ++ (String.fromInt <| (List.length model.filteredTruckList))
                                    ,el [Element.alignBottom, pdr 5] <| textValue <| "Total page trucks found : " ++ (String.fromInt <| (List.length model.pagedTruckList))
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
                                                                                Array.toList uiModel.sleeperBunkFilters
                                                                            ]
                            ]
                            ,row[ wf, bwb 0, pd 0][
                                wrappedRow[  wf,  bw 0, pd 0 ]
                                    --<| getPageNumbersList  model uiModel  -- use this style to skip parans...
                                    <| (getPageNumbersList  model uiModel)
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

getNumberList model uiModel =
    let
        --lng = String.split "." (String.fromFloat (Basics.toFloat ( List.length model.filteredTruckList  )/ 100))
        (pageNumberIntPositionPart, pageNumberDecimalPositionPart) =
            model.filteredTruckList
                |> List.length
                |> Basics.toFloat
                |> (\flt -> flt / 100)
                |> String.fromFloat
                |> String.split "."
                |> (\brokenStrList -> 
                                   
                                    (
                                        case List.head brokenStrList of
                                                    Just val -> case String.toInt val of 
                                                                    Just num -> num 
                                                                    Nothing -> 0
                                                    Nothing -> 0
                                        ,
                                            if List.length brokenStrList > 1 then
                                                case List.head << List.reverse <| brokenStrList of
                                                        Just val -> case String.toInt val of 
                                                                        Just num -> num 
                                                                        Nothing -> 0
                                                        Nothing -> 0
                                            else
                                                0
                                    )

                                -- let -- this is fine too
                                --     first = case List.head strList of
                                --                 Just val -> val
                                --                 Nothing -> ""
                                --     second = case List.head << List.reverse <| strList of
                                --                 Just val -> val
                                --                 Nothing -> ""
                                -- in
                                --     (first, second)
                                 
                )

        totalPages = pageNumberIntPositionPart + if pageNumberDecimalPositionPart > 0 then 1 else 0
    in
        (List.range 1  <| if totalPages == 1 then 0 else totalPages, totalPages)

getPageNumbersList  model uiModel  = 
    let
        (pageNumbers, totalPages) =  getNumberList  model uiModel
        
 
    in
    
        List.map (\num -> 
                
                    row[pd 0, bw 0,wpx 35, hpx 35]
                                [
                                    --el [pd 5, wf,  bw 1, bc 95 95 95,fc  250 250 250, Font.size 16 ] <| textValue <| String.fromInt num
                                    --Input.button [pd 5, wf,  bw 1, bc 95 95 95,fc  250 250 250, Font.size 16 ] <| textValue <| String.fromInt num
                                    Input.button [pd 5, wf,  bw 1, bc 95 95 95,fc  250 250 250, Font.size 16 ]
                                        { 
                                            onPress = Just (PageNumberClicked num  totalPages) --Just (PageNumberClicked (((num - 1) * 100) + 1)  totalPages)
                                            ,label = textValue <| String.fromInt num
                                        }
                                ]

                ) pageNumbers -- use this style to skip parans...


---- PROGRAM ----


main : Program OnLoadSearchFilter (Model,UIModel) Msg
main =
    Browser.element
        { view = view
        , init = init -- to capture flags from JS
        , update = update
        , subscriptions = always Sub.none
        }
