module TruckViews.Truck exposing (..)

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

import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Helpers.Utils exposing (..)

trucksView  : List Truck -> Element Msg
trucksView trucks =
    trucks
        |> List.indexedMap truckView
        |> column [hf, wf, spy 5]

truckView  : Int -> Truck -> Element Msg
truckView index truck =
    row[bw new, wf, pd 5, bc 231 231 231] --bc 47 48 49
    [
        column[wf]
        [
             image [alignLeft, bw one, wfp 3, pdl 5] {src = "https://az832863.vo.msecnd.net/~/media/images/trucks/i0414681/i0414681_1.jpg?_=-1039260339&mw=2048&thn=0&w=1024", description ="Logo" }
        ]
        ,
        column[wfp 3]
        [
            textValue <| (String.fromInt <| index) ++ " - " ++  truck.cdl ++ " - " ++  truck.make ++ " - " ++  (String.fromInt <| truck.year) ++ " - APU - " ++ truck.apu
        ]
        ,
        column[wf]
        []
    ]






          