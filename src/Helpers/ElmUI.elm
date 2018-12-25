module Helpers.ElmUI exposing (..)

import Element exposing (htmlAttribute, text, image, el, none, scrollbarY)
import Helpers.ElmStyleShotcuts exposing (..)
import Html.Attributes exposing (id)
import Element.Font as Font

eId idValue
    = htmlAttribute (id idValue)

textValue stringValue
    = text <| stringValue

buildCollapseAllImage userAction =
    if userAction == True then 
        image [hpx 14, bw one] {src = "collapse.png", description ="Logo" }
    else 
        image [hpx 14, bw one] {src = "expand.png", description ="Logo" }

expandCollapseAll state =
    
        if state then
            [hf,  pd 10]
        else
            [hfmax 0, pd 0, scrollbarY]
            --bw 1

buildChkBoxImage userAction =
        if userAction == True then 
            image [hpx 16] {src = "checked.png", description ="Logo" }
        else 
            el [hpx 16, wpx 16, bw 2,brc 101 101 101, br 3] <| none --this put empty square, unchecked checkbox style

one = 0

two = 0

new = 1