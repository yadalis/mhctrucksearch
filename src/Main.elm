module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Commands exposing (..)
import RemoteData  exposing (..)
import Http exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)

---- INIT ----

type alias SearchFilter =
    String

init : SearchFilter -> (Model, Cmd Msg)
init jsflg =
    ( { trucks = Loading }
    , fetchTrucks
    )


---- UPDATE ----

update : Msg -> Model -> ( Model, Cmd Msg  )
update msg model =
    case msg of
        OnFetchTrucks response ->
            ( { model | trucks = response }, Cmd.none )

---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ,case model.trucks of
                RemoteData.NotAsked ->
                    text ""

                RemoteData.Loading ->
                    text "Loading..."

                RemoteData.Success inProcessRORows ->
                    --text <| String.fromInt (List.length inProcessRORows)
                    buildROstable inProcessRORows

                RemoteData.Failure error ->
                    text <| 
                            case error of
                                Http.BadUrl url ->
                                  String.toLower "( Err (Http.BadUrl url) )"

                                Http.Timeout ->
                                    String.toLower "Err Http.Timeout"

                                Http.NetworkError ->
                                    String.toLower "Err Http.NetworkError"

                                Http.BadStatus code ->
                                    String.toLower "Err (Http.BadStatus metadata.statusCode) " ++ String.fromInt code
                                
                                Http.BadBody code ->
                                    String.toLower "Err (Http.BadBody metadata.statusCode)" ++  code
 
        ]



buildROstable : List Truck -> (Html Msg)
buildROstable trucks =

    div [id "searchContainer"]
     [  
        
        rolegend
        ,table [ class "item-list", id "searchList" ]
        [ thead []
            [ tr []
                [ th [ attribute "width" "25px" ]
                    [ text "Priority" ]
                , th [ attribute "width" "25px" ]
                    [ text "Sev"               ]
                , th [ attribute "width" "25px" ]
                    [ text "B"               ]
                , th [ attribute "width" "25px" ]
                    [ text "D"               ]
                , th [ attribute "width" "25px" ]
                    [ text "Bay"               ]
                , th [ attribute "width" "75px" ]
                    [ text "RO#"               ]
                , th [ attribute "width" "30px" ]
                    [ text "Cust#"               ]
                , th [ attribute "width" "220px" ]
                    [ text "Customer"               ]
                , th [ attribute "width" "35px" ]
                    [ text "Unit"               ]
                , th [ attribute "width" "35px" ]
                    [ text "VIN"               ]
                , th [ attribute "width" "100px" ]
                    [ text "Status"               ]
                , th []
                    [ text "Updated"               ]
                , th [ attribute "width" "75px" ]
                    [ text "Tech#1"               ]
                , th [ attribute "width" "50px" ]
                    [ text "#2"               ]
                , th [ attribute "width" "25px" ]
                    [ text "DW"               ]
                , th [ attribute "width" "75px" ]
                    [ text "ETC"               ]
                ]
            ]
        , tbody []  (List.indexedMap itemViewView trucks ) 
        ]
   ]



rolegend : Html Msg
rolegend =
    div [class "dashboard-legend"]
    [   
       p [][text "Key:"]
        ,ul[]
         [
             li [ class "dashboard-legend-blue" ]
                [
                    span [][] 
                    ,text " Waiting on Parts/Sublet" 
                ]
            ,li [ class "dashboard-legend-green" ]
                [ span [][] 
                    ,text " Parts/Sublet Arrival Time" ]
            ,li [ class "dashboard-legend-yellow" ]
                [ span [][] 
                    ,text " Work in Process/Over Estimate" ] 
            ,li [ class "dashboard-legend-red" ]
                [ span [][] 
                    ,text " Past ETC" ]
            ,li [ class "dashboard-legend-bolditalic" ]
                [ span [] [ text "Bold Italicized" ]
                  ,text "= Portal User" 
                ]
         ]
    ]


itemViewView : Int -> Truck -> Html Msg
itemViewView index truck  =

         tr [   ]
            [ 
                td []
                    [
                        text <| String.fromInt index
                    ]
                ,td []
                    [ 
                        text <| truck.id
                    ]
                ,td []
                    [ 
                        text <| truck.name
                    ]
                ,td[]
                    
                    (List.map (\ t -> tr[] [ td[] [ text <| t.value], td[] [ text <| ">" ++ t.displayName]  ] ) truck.fields)
            ]

        

---- PROGRAM ----


main : Program SearchFilter Model Msg
main =
    Browser.element
        { view = view
        , init = init -- to capture flags from JS
        , update = update
        , subscriptions = always Sub.none
        }
