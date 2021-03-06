module Main_gray_Styles_test exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)
import Msg exposing (..)

--import RemoteData  exposing (..)
import Element exposing (..)
import Element.Input as Input exposing (..) 
import Element.Font as Font exposing (..)

import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)

import Array exposing(..)
import Html.Events.Extra as ExtraHtmlEvents

import Element.Lazy as Lazy exposing(..)

import List.Extra exposing (..)

import Element.Events exposing (..)
import Helpers.Utils exposing (..)
import Browser.Dom exposing (..)

---- INIT ----

type alias OnLoadSearchFilter =
    String

init : OnLoadSearchFilter -> ( (Model, UIModel) , Cmd Msg)
init jsflg =
    ( (initialModel,initalUIModel jsflg)
        , Cmd.none
        --, Cmd.batch [fetchTrucks, fetchSearchFilterRanges] -- this executes all commands in async manner, it seems ?
    )

---- UPDATE ----
 
update : Msg -> (Model, UIModel) -> ( (Model, UIModel) , Cmd Msg  )
update msg (model, uiModel) =
    case msg of
         SearchString str ->
            ((model, uiModel), Cmd.none)

focusStyle : Element.Option
focusStyle =
    Element.focusStyle
        { borderColor = Nothing
        , backgroundColor = Nothing
        , shadow = Nothing
        }

view : (Model, UIModel) -> Html Msg
view (model, uiModel) =

       layoutWith {options = [focusStyle]}  [ 
                                            Font.family
                                                [ Font.external
                                                    { name = "Roboto"
                                                    , url = "https://fonts.googleapis.com/css?family=Roboto"
                                                    }
                                                , Font.sansSerif
                                                ] 
                                        ] 
                <| viewFunc
                    

viewFunc =
    column[ bw 0,brc 250 250 250,  bc 125 125 125, wf, hf, pd 5]
    [
        row[bw 0, wf, spx 5]
        [
            column[wpx 300, bw 0]
            [
                image [hpx 38] {src = "mhclogo.png", description ="Logo" }
            ]
            ,
            column[wf, bw 0]
            [
                
            ]
        ],
        row[wf, hf, spx 5]
        [
            column[wpx 300, bc 77 77 77, hf, spy 25, pde 15 5 5 5 ]
            [
                row[ bw 0, spx 25, eacx,  fc 250 250 250][
                            Input.button ( [ bw 0,     pdl 0, fs 12, mouseOver [fc 200 200 200] ])
                            { 
                                onPress = Nothing
                                ,label = el[  bwb 0, fb] <| textValue  "EXPAND ALL"
                            }
                            ,
                            Input.button ( [ bw 0,      pdl 0, fs 12,  mouseOver [fc 200 200 200] ])
                            { 
                                onPress = Nothing
                                ,label = el[  bwb 0, fb] <| textValue "COLLAPSE ALL"
                            }
                            ,
                            Input.button ( [  bw 0,     pdl 0, fs 12,  mouseOver [fc 200 200 200] ])
                            { 
                                onPress =  Nothing
                                ,label = el[  bwb 0, fb] <| textValue "CLEAR FILTERS"
                            }
                        ]                
            ]
            ,
            column[wf, bc 77 77 77, hf]
            [

            ]
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