module Main exposing (..)

import Svg exposing (..)
import Svg.Attributes as A
import Html.App as App
import Platform exposing (Program)
import GenericDict as Dict
import Time exposing (every, millisecond)


type alias Coord =
    ( Int, Int )


type Cell
    = Head
    | Tail
    | Conductor


type alias Grid =
    Dict.GenericDict Coord Cell


type alias Model =
    { grid : Grid
    }


type Msg
    = Tick


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


compareCoord : Coord -> Coord -> Order
compareCoord ( ax, ay ) ( bx, by ) =
    case compare ax bx of
        LT ->
            LT

        EQ ->
            compare ay by

        GT ->
            GT


init : ( Model, Cmd Msg )
init =
    ( { grid =
            Dict.fromList compareCoord
                [ ( ( 1, 2 ), Head )
                , ( ( 0, 2 ), Tail )
                , ( ( -1, 2 ), Conductor )
                , ( ( 2, -1 ), Conductor )
                , ( ( 2, 0 ), Conductor )
                , ( ( 2, 1 ), Conductor )
                , ( ( 1, -2 ), Conductor )
                , ( ( 0, -2 ), Conductor )
                , ( ( -1, -2 ), Conductor )
                , ( ( -2, -1 ), Conductor )
                , ( ( -2, 0 ), Conductor )
                , ( ( -2, 1 ), Conductor )
                ]
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model | grid = tick model.grid }, Cmd.none )


tick : Grid -> Grid
tick grid =
    Dict.map (tickCell grid) grid


tickCell : Grid -> Coord -> Cell -> Cell
tickCell grid ( x, y ) cell =
    case cell of
        Head ->
            Tail

        Tail ->
            Conductor

        Conductor ->
            let
                heads =
                    cellNeighbours grid ( x, y )
                        |> List.filter ((==) Head)
                        |> List.length
            in
                if heads == 1 || heads == 2 then
                    Head
                else
                    Conductor


cellNeighbours : Grid -> Coord -> List Cell
cellNeighbours grid ( x, y ) =
    let
        offsets =
            [ ( 1, 1 )
            , ( 0, 1 )
            , ( -1, 1 )
            , ( -1, 0 )
            , ( -1, -1 )
            , ( 0, -1 )
            , ( 1, -1 )
            , ( 1, 0 )
            ]
    in
        offsets
            |> List.filterMap (\( a, b ) -> Dict.get ( x + a, y + b ) grid)


subscriptions : Model -> Sub Msg
subscriptions model =
    every (500 * millisecond) (\_ -> Tick)


cellFill : Cell -> String
cellFill cell =
    case cell of
        Head ->
            "red"

        Tail ->
            "orange"

        Conductor ->
            "yellow"


viewCell : ( Coord, Cell ) -> Svg a
viewCell ( ( x, y ), cell ) =
    rect
        [ A.x (toString (x * 20))
        , A.y (toString (y * 20))
        , A.width "20"
        , A.height "20"
        , A.fill (cellFill cell)
        ]
        []


viewGrid : Grid -> Svg a
viewGrid grid =
    g [ A.transform "translate(250, 250)" ] (Dict.toList grid |> List.map viewCell)


view : Model -> Svg a
view model =
    svg
        [ A.style "background-color: #000000"
        , A.width "500"
        , A.height "500"
        ]
        [ viewGrid model.grid ]
