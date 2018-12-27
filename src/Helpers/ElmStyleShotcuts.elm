module Helpers.ElmStyleShotcuts exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font




edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }

--Height
hf = 
   height fill
hfp number = 
    height <| fillPortion number
hpx number
    = height <| px number
hfmin minValue
   = height (fill
        |> minimum minValue
    )
hfmax maxValue
   = height (fill
        |> maximum maxValue
    )
hfRange minValue maxValue
   = height (fill
        |> minimum minValue
        |> maximum maxValue
    )

--Width
wf = 
   width fill
wfp number 
    = width <| fillPortion number
wpx number
    = width <| px number

wfmin minValue
   = width (fill
        |> minimum minValue
    )
wfmax maxValue
   = width (fill
        |> maximum maxValue
    )
wfRange minValue maxValue
   = width (fill
        |> minimum minValue
        |> maximum maxValue
    )

--Spacing
sp number
    = spacing number
spx number
    = spacingXY number 0
spy number
    = spacingXY 0 number

-- Border
bw number
    = Border.width number
bwe top right bottom left
    = Border.widthEach    {top = top, right = right, bottom = bottom, left = left}

bwx number
    = Border.widthXY number 0
bwy number
    = Border.widthXY 0 number

bwt number
    = Border.widthEach    {edges | top = number}
bwr number
    = Border.widthEach    {edges | right = number}
bwb number
    = Border.widthEach    {edges | bottom = number}
bwl number
    = Border.widthEach    {edges | left = number}

bdot =
    Border.dotted

brc red green blue 
    = Border.color <| rgb255 red green blue
br number
    = Border.rounded number
bre topLeft topRight bottomLeft bottomRight
    = Border.roundEach   {topLeft = topLeft, topRight = topRight, bottomLeft = bottomLeft, bottomRight = bottomRight}

--Background
bc red green blue 
    = Background.color <| rgb255 red green blue

--Padding

p16 = padding 16
px16 = paddingXY 16 0
py16 = paddingXY 0 16
pdt16
    = paddingEach    {edges | top = 16}
pdr16
    = paddingEach    {edges | right = 16}
pdb16
    = paddingEach    {edges | bottom = 16}
pdl16
    = paddingEach    {edges | left = 16}


pd number
    = padding number
pde top right bottom left
    = paddingEach    {top = top, right = right, bottom = bottom, left = left}

pdx number
    = paddingXY number 0
pdy number
    = paddingXY 0 number

pdt number
    = paddingEach    {edges | top = number}
pdr number
    = paddingEach    {edges | right = number}
pdb number
    = paddingEach    {edges | bottom = number}
pdl number
    = paddingEach    {edges | left = number}


--Font Alignments
fal 
    = Font.alignLeft
far 
    = Font.alignRight
fac 
    = Font.center

fc red green blue 
    = Font.color <| rgb255 red green blue

fs num =
    Font.size num

fl =
    Font.light

fb =
    Font.bold

feb =
    Font.extraBold

fh= 
    Font.hairline

fr =
    Font.regular
   
--Element Alignments
eat =
    Element.alignTop
ear = 
    Element.alignRight
eab =
    Element.alignBottom
eal = 
    Element.alignLeft

eacx =
    Element.centerX
eacy =
    Element.centerY
