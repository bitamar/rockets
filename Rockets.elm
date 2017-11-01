module Rockets exposing (main)

import AnimationFrame
import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import List.Extra as List
import Random
import Task
import Time exposing (Time)


type alias Rocket =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , angle : Float
    , thrusters : ( Float, Float )
    , fuel : Float
    , ticksClock : Int
    , plan : Plan
    }


type alias Plan =
    Dict Int ( Float, Float )


newRocket : Plan -> Rocket
newRocket plan =
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , angle = 0
    , thrusters = ( 250, 250 )
    , fuel = 1200
    , ticksClock = 0
    , plan = plan
    }


type alias Model =
    List Rocket


initialModel : Model
initialModel =
    []


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, generateRocket )
        , update = update
        , view = view
        , subscriptions = \model -> AnimationFrame.diffs (Tick << Time.inSeconds)
        }


type Msg
    = NewRocket (List Float)
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRocket randomNumbers ->
            let
                changesAmount =
                    List.head randomNumbers
                        |> Maybe.withDefault 0
                        |> (\n -> n / 100)
                        |> floor

                changePoints =
                    0
                        :: List.take (changesAmount - 1) randomNumbers
                        |> List.map (\n -> floor <| n / 10)

                thrusts =
                    List.drop changesAmount randomNumbers
                        |> List.splitAt changesAmount
                        |> (\( l, r ) -> List.zip l r)

                plan =
                    Dict.fromList <| List.zip changePoints thrusts

                updatedModel =
                    newRocket plan :: model
            in
            updatedModel ! []

        Tick time ->
            let
                updatedRockets =
                    model
                        |> List.map (updateRocket time)
                        |> List.filter (\rocket -> rocket.y >= 0)

                cmd =
                    if List.isEmpty model then
                        generateRocket
                    else
                        Cmd.none
            in
            ( updatedRockets, cmd )


updateRocket : Time -> Rocket -> Rocket
updateRocket time rocket =
    let
        noFuel =
            rocket.fuel <= 0

        ( leftThruster, rightThruster ) =
            rocket.thrusters

        yThrust =
            if noFuel then
                0
            else
                time * 0.7 * (leftThruster + rightThruster)

        xThrust =
            if noFuel then
                0
            else
                time * 0.3 * (leftThruster - rightThruster)

        vy =
            rocket.vy - (time * 308) + yThrust

        updatedThrusters =
            Dict.get rocket.ticksClock rocket.plan |> Maybe.withDefault rocket.thrusters

        updatedRocket =
            { rocket
                | x = rocket.x + time * rocket.vx
                , y = rocket.y + time * vy
                , vy = vy
                , vx = rocket.vx + xThrust
                , angle = atan2 rocket.vx vy
                , fuel = rocket.fuel - time * (leftThruster + rightThruster)
                , thrusters =
                    if noFuel then
                        ( 0, 0 )
                    else
                        updatedThrusters
                , ticksClock = rocket.ticksClock + 1
            }
    in
    updatedRocket


generateRocket : Cmd Msg
generateRocket =
    Random.generate NewRocket <| Random.list 40 <| Random.float 0 1000


view : Model -> Html msg
view model =
    div []
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "static.css" ] []
        , div [] <| List.map viewRocket model
        , div [] [ text <| toString <| List.head model ]
        ]


viewRocket : Rocket -> Html msg
viewRocket rocket =
    let
        width =
            10

        left =
            "calc(50% - " ++ toString (width / 2) ++ "px + " ++ toString rocket.x ++ "px)"

        rocketStyle =
            [ ( "background-color", "lightgrey" )
            , ( "width", toString width ++ "px" )
            , ( "height", "40px" )
            , ( "position", "absolute" )
            , ( "bottom", toString rocket.y ++ "px" )
            , ( "left", left )
            , ( "border-radius", "50% 50% 0 0" )
            , ( "transform", "rotate(" ++ toString rocket.angle ++ "rad)" )
            ]

        thrustStyle side =
            let
                rotation =
                    if side == "left" then
                        "5"
                    else
                        "-5"
            in
            [ ( "border-radius", "50% 50% 0 0" )
            , ( "border-bottom", "1px solid #555" )
            , ( "position", "absolute" )
            , ( "background-color", "#aaa" )
            , ( "width", toString (width / 2.5) ++ "px" )
            , ( "height", "15px" )
            , ( "bottom", "-2px" )
            , ( "transform", "rotate(" ++ rotation ++ "deg)" )
            , ( side, "-2px" )
            ]

        -- colors =
        --     [ "rgba(254, 252, 201, 0.7)"
        --     , "rgba(254, 236, 133, 0.7)"
        --     , "rgba(255, 174, 52, 0.7)"
        --     , "rgba(236, 118, 12, 0.7)"
        --     , "rgba(205, 70, 6, 0.7)"
        --     , "rgba(151, 55, 22, 0.7)"
        --     , "rgba(69, 27, 14, 0.7)"
        --     ]
        --
        fire thruster =
            let
                fireShadow =
                    "0 2px 2px rgb(254,252,201), 1px 2px 3px rgb(254,236,133), -1px 4px 4px rgb(255,174,52), 1px 6px 5px rgb(236,118,12), -1px 8px 6px rgb(205,70,6), 0 9px 7px rgb(151,55,22), 1px 10px 8px rgb(69,27,14)"

                fireDistance =
                    ceiling <| thruster / 90

                zoom =
                    1 + thruster / 1000

                fireStyle =
                    [ ( "box-shadow", fireShadow )
                    , ( "top", toString fireDistance ++ "px" )
                    , ( "zoom", toString zoom )
                    ]
            in
            if thruster > 0 then
                div [ class "fire", style fireStyle ] []
            else
                text ""

        ( leftThruster, rightThruster ) =
            rocket.thrusters
    in
    div [ class "rocket", style rocketStyle ]
        [ div [ class "stripe" ] [ text "⌒" ]
        , div [ class "thrust", style <| thrustStyle "left" ] [ fire leftThruster ]
        , div [ class "thrust", style <| thrustStyle "right" ] [ fire rightThruster ]
        ]
