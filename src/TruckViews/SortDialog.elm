module TruckViews.SortDialog exposing (..)

import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Element exposing (..)
import Element.Font as Font exposing (..)
import Msg exposing (..)

showSortOptionsDialog : Bool -> Element Msg
showSortOptionsDialog show =
    if show then 
        column[bc 245 245 245, pd 15, wpx 400, hpx 400, br 5, bw 1]
        [
            row[Element.alignRight, bw 2, pd 5, wf][
                    textValue "x"
            ],

            row[wf, hf, Font.underline][
                column[spy 5][
            textValue "Asdfasdfasasdf",
            textValue "Asdfasdfasasdf",
            textValue "Asdfasdfasasdf",
            textValue "Asdfasdfasasdf",

            textValue "Asdfasdfasasdf",
            textValue "Asdfasdfasasdf",
            textValue "Asdfasdfasasdf",
            textValue "Asdfasdfasasdf",
            textValue "Asdfasdfasasdf",
            textValue "Asdfasdfasasdf",

            textValue "Asdfasdfasasdf"
                ]
            ]
        ]
        
    else
        none



