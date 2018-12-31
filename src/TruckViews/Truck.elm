module TruckViews.Truck exposing (..)

import Html.Attributes exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Element exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import BusinessFunctions.TruckFunctions exposing (..)
import Numeral exposing(format, formatWithLanguage)
import Url.Builder exposing (..)
import Helpers.Colors exposing (..)

trucksView  : List Truck -> Element Msg
trucksView trucks =
    trucks
        |> List.indexedMap truckView
        |> column [hf, wf, spy 5]

truckView  : Int -> Truck -> Element Msg
truckView index truck =
    let
        isTruckSold  =
            truck.salesStatusFlag == "I" || truck.salesStatusFlag == "S"

        formatImageLink = -- "https://az832863.vo.msecnd.net/~/media/images/trucks/i0412861/i0412861_1.jpg?_=-1254660955&mw=2048&thn=0&w=225"
                            truck.primaryImageLink
                                                |> String.isEmpty
                                                |> (\isLinkEmpty -> if isLinkEmpty then "photoscomingsoon.png"  else truck.primaryImageLink)
                                                |> String.replace "&h=16" ""
                                                |> String.replace "&w=16" "&w=225" 
                                                |> String.replace "&thn=1" "&thn=2"
    in
    
        row[wf, pd 5, greyBg 240 ]
        [
            column[hf]
            [
                image [ hpx 110,wpx 125, greyBg 250
                    ,
                    inFront (
                        if isTruckSold then
                            ( el[alignBottom, greyFont 250, bc 234 67 82, wf, alpha 0.55, feb, fac] <| textValue "SOLD" )
                        else
                            none
                    )
                        ] {src = formatImageLink, description ="Logo" }
            ]
            ,
            column[wf, hf, pde 3 3 3 15, spy 10]
            [
                row[wf]
                [
                    link [ Element.htmlAttribute (target "_blank")]
                        { url = 
                            crossOrigin "https://www.mhc.com/trucks/used" [truck.year, truck.make, truck.model, ((\tup -> Tuple.second tup ) <| buildTruckIdNumber truck)] []
                            , label = el [fs 24, fb, mhcRed] <| textValue <| truck.title
                        }
                    -- ,
                    -- link [wf,  Element.htmlAttribute (target "_blank"), fal ]
                    --     { url = 
                    --         formatImageLink
                    --         , label = paragraph [fs 28, fb, mhcRed] [textValue <| formatImageLink]
                    --     }

                ]
                ,row[bw 0, spx 15]
                [
                    el [fs 24, fb, greyFont 68, wpx 155] <| textValue <| buildPriceValue truck.price                        
                    ,
                    if truck.stockNumber == 0 && String.trim truck.truckType /= "P" then
                        el [fs 22, greyFont 167,  bw 0] <| textValue <| "Appraisal# - " ++ String.fromInt truck.appraisalNumber
                    else
                        none
                ]
                ,row[hf, wf, fs 16, bw 0]
                [
                    column[bw 0, wfmax 300, hf, pd 0, spy 5]
                    [
                        dataFieldView "Location:" <| 
                                                    if String.isEmpty truck.locationName then
                                                        "N/A" 
                                                    else
                                                        truck.locationName

                        ,dataFieldView "Stock#:" <| if truck.stockNumber == 0 then "N/A" else String.fromInt truck.stockNumber
                        ,dataFieldView "Chassis#:" <| String.dropLeft 9 truck.chassisNumber
                    ]
                    ,column[bw 0,   wfmax 250, hf, pd 0, spy 5]
                    [
                        dataFieldView "Mileage:" <|  format "0,0" <|  truck.mileage
                        ,dataFieldView "Sleeper Size:" <| 
                                                        if truck.sleeperInches ==  0 then
                                                            "Non-Sleeper"
                                                        else
                                                            String.fromFloat <| truck.sleeperInches
                        ,dataFieldView  "Horsepower:" <| String.fromFloat truck.engineHP
                    ]
                    ,column[bw 0,wf, hf, pd 0, spy 5]
                    [
                        dataFieldView  "Engine Make:"   truck.engineMake
                        ,dataFieldView  "Engine Model:"   truck.engineModel
                        ,dataFieldView  "Transmission:"    truck.transType
                    ]
                ]
            ]
            -- show comments may be ? or use this for extra features like pop open for full details
            -- column[wfp 4, bw 2, hf, pdl 15]
            -- []
        ]

dataFieldView fieldName fieldValue =
    row[bw 0, wf]
    [
        paragraph[fal,spy 1, fs 18,  greyFont 105]
        [
            el[fb ] <| textValue <| fieldName
            , el[pdl 5, fs 16, greyFont 97] <| textValue <| fieldValue
        ]   
    ]

buildPriceValue : Float -> String
buildPriceValue price =
    if price == 0 then
        "Call for pricing"
    else
       format "$0,0" <| price