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
import Element.Font as Font exposing (..) 

import TruckViews.Truck exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Helpers.Utils exposing (..)
import Helpers.TruckFunctions exposing (..)
import Task
import Array exposing(..)

---- INIT ----

type alias SearchFilter =
    String

init : SearchFilter -> ( (Model, UIModel) , Cmd Msg)
init jsflg =
    ( (initialModel,initalUIModel)
        , fetchTrucks
       -- , Cmd.none
    )


---- UPDATE ----

update : Msg -> (Model, UIModel) -> ( (Model, UIModel) , Cmd Msg  )
update msg (model, uiModel) =
    case msg of
        OnFetchTrucks response ->
            --( ({ model | trucks = response }, uiModel), sendMessage UnwrapWebDataTrucks )
            let
                trucks = case response of
                                    Ok truckList ->
                                            -- (Array.fromList
                                            --     [
                                            --         Question  Nothing "Is Elm the best UI Language "           "True"   ["True", "False"] Nothing --["A1","A2","A3","A4","A5"]
                                            --         ,Question Nothing "Does Elm supports Native Mobile apps " "False"   ["True", "False"] Nothing --["B1","B2","B3","B4","B5"]
                                            --         ,Question Nothing "Has Elm invented by MHC "               "False"  ["True", "False"] Nothing --["D1","D2","D3","D4","D5"] 
                                            --     ]
                                            -- )
                                            truckList -- questions from the http.get
                                    Err err ->
                                            --Array.empty
                                            []
            in
                ( ({ model | truckList = trucks, textVal = "data loaded" }, uiModel), Cmd.none)
            --( ({ model | truckList = trucks }, uiModel), sendMessage UnwrapWebDataTrucks )
        -- UnwrapWebDataTrucks ->
        --     let
        --         decodedTrucks = case model.truckList of
        --                                 RemoteData.NotAsked ->
        --                                     Array.empty
        --                                 RemoteData.Loading ->
        --                                     Array.empty
        --                                 RemoteData.Success trucks ->
        --                                     --text <| String.fromInt (List.length inProcessRORows)
        --                                     --List.indexedMap 
        --                                     --searchFiltersView trucks
        --                                     trucks
        --                                 RemoteData.Failure error ->
        --                                     Array.empty
        --     in
        --       ( ({ model | truckList = decodedTrucks }, uiModel), Cmd.none )
        FilterCDLNoCheckBoxClicked userAction ->
            ( (model, {uiModel | filterCDLNoSelected = userAction}), Cmd.none )
        FilterCDLYesCheckBoxClicked userAction ->
            --( (model, uiModel), Cmd.none )
            ( (model, {uiModel | filterCDLYesSelected = userAction}), Cmd.none )
        SearchString searchString ->
            ( (model, {uiModel | searchString = searchString}), Cmd.none )
        SearchPressed ->
            ( (model, uiModel), Cmd.none )

---- VIEW ----


view : (Model, UIModel) -> Html Msg
view (model, uiModel) =
    --let
        -- (element, truckList) = case model.trucks of
        --         RemoteData.NotAsked ->
        --             (textValue "", [])

        --         RemoteData.Loading ->
        --             ( image [hpx 18, bw one, wf, pdl 5] {src = "loader.gif", description ="Logo" }, [])

        --         RemoteData.Success trucks ->
        --             --text <| String.fromInt (List.length inProcessRORows)
        --             --List.indexedMap 
        --             --searchFiltersView trucks
        --             (textValue "", trucks)

        --         RemoteData.Failure error ->
        --            ( textValue <| 
        --                     case error of
        --                         Http.BadUrl url ->
        --                           String.toLower "( Err (Http.BadUrl url) )"

        --                         Http.Timeout ->
        --                             String.toLower "Err Http.Timeout"

        --                         Http.NetworkError ->
        --                             String.toLower "Err Http.NetworkError"

        --                         Http.BadStatus code ->
        --                             String.toLower "Err (Http.BadStatus metadata.statusCode) " ++ String.fromInt code
                                
        --                         Http.BadBody code ->
        --                             String.toLower "Err (Http.BadBody metadata.statusCode)" ++  code
        --             , [])
    --in
        let
                    printEstimateBtnStyle = if String.length uiModel.searchString > 0 then 
                                    [ bc 226 63 63, fc 250 250 250] 
                                else
                                    [ bc 198 201 206, fc 245 245 245]
        in
        
        layout [
            
            inFront(
                         row[wf, hpx 75, bc 47 48 49, fc 250 250 250, alpha  0.95]
                                [
                                    image [hpx 32, bw one] {src = "https://az832863.vo.msecnd.net/~/media/images/components/pagelogos/mhclogo.png?_=-381616326&h=61", description ="Logo" }
                                    ,
                                    el [pdl 25] <| textValue "Truck Search - Powerfull-Flexible search app"
                                ] 
                    )

            ,hf, pd 0 ] <| 
            
                            -- inFront( row[wf, hpx 75, bc 47 48 49, fc 250 250 250]
                            --     [el [centerX, Font.extraBold, Font.heavy] <| textValue "MHC Truck Search - Power/Flexible search app"] )
                            -- ,
                            row[hf,wf, pdt 76]
                            [
                                -- (
                                --     if List.length model.truckList > 0 then
                                --         (   
                                --             List.map (\t -> textValue <| t.title ) model.truckList )
                                --     else
                                --     [  image [hpx 18, bw one, wf, pdl 5, bwb 2] {src = "loader.gif", description ="Logo" } ] 
                                -- )
                                --List.map (\t -> (textValue <| t.title) ) model.truckList )
                                --textValue <| model.textVal ++ (String.fromInt <| List.length model.truckList)
                                -- row[wf, bw one]
                                -- --[element]
                                --     [none]
                                -- ,
                                
                                        column
                                        [  hf
                                        , wf
                                        , pde 5 10 0 10
                                        , bc 225 225 225
                                        , spy 25
                                        
                                        ]
                                        
                                            
                                            --(List.map (\cdl -> textValue cdl) (buildCDLValueGroups truckList))
                                            [
                                                
                                                row[wf, pdt 15][ 
                                                                Input.text [wf, hf]
                                                                {
                                                                    onChange = SearchString
                                                                    ,text  = uiModel.searchString
                                                                    , label = labelLeft [] none --(el [] <| textValue chkboxLabel)
                                                                    , placeholder = Just (Input.placeholder [] (el [] <| textValue "Search trucks"))
                                                                }
                                                                ,
                                                                  Input.button ( [ hf, wpx 75, eId "prntEst"] ++ printEstimateBtnStyle)
                                                                    { onPress = if String.length uiModel.searchString > 0 then Just SearchPressed else Nothing
                                                                    , label = 
                                                                            if String.length uiModel.searchString > 0 then 
                                                                                image [hpx 32, bw one] {src = "srch_white.ico", description ="Logo" }
                                                                            else
                                                                                image [hpx 32, bw one] {src = "srch_grey.ico", description ="Logo" }
                                                                    }
                                                
                                                ]
                                                ,
                                                if List.length model.truckList > 0 then
                                                    (buildCDLValueGroups uiModel model.truckList)
                                                else
                                                    textValue <| "Loading trucks, please wait... "
                                                
                                                
                                            ]

                                        ,column[hf, wfp 5, bc 205 205 205, bwl 0, pdl 15 ]
                                            [
                                                row[wf, bw 0, hpx 100, pde 10 10 10 0][ el [alignTop] <| textValue <| "Selected Filters... "]
                                                ,
                                                column[wf, scrollbarY]
                                                [
                                                    trucksView model.truckList 
                                                ]
                                                
                                            ]

                                
                            ]
            
---- PROGRAM ----


main : Program SearchFilter (Model,UIModel) Msg
main =
    Browser.element
        { view = view
        , init = init -- to capture flags from JS
        , update = update
        , subscriptions = always Sub.none
        }
