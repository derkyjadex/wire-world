module Main exposing (..)

import Svg exposing (..)
import Svg.Attributes as A
import Svg.Events as E
import Html.App as App
import Platform exposing (Program)
import GenericDict as Dict
import Time exposing (every, millisecond)
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode exposing (Decoder)
import Ports exposing (..)


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
    | Click Coord
    | Save
    | Load
    | OnLoad String


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

        Click coord ->
            let
                grid =
                    case Dict.get coord model.grid of
                        Nothing ->
                            Dict.insert coord Head model.grid

                        Just _ ->
                            Dict.remove coord model.grid
            in
                ( { model | grid = grid }, Cmd.none )

        Save ->
            ( model, save (Encode.encode 0 (encodeGrid model.grid)) )

        Load ->
            ( model, load () )

        OnLoad s ->
            case Decode.decodeString decodeGrid s of
                Ok grid ->
                    ( { model | grid = grid }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


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
    Sub.batch
        [ every (50 * millisecond) (\_ -> Tick)
        , onLoad OnLoad
        ]


encodeGrid : Grid -> Encode.Value
encodeGrid grid =
    Encode.list
        (Dict.toList grid
            |> List.map
                (\( ( x, y ), cell ) ->
                    Encode.list
                        [ Encode.int x
                        , Encode.int y
                        , Encode.string (toString cell)
                        ]
                )
        )


decodeGrid : Decoder Grid
decodeGrid =
    Decode.map (Dict.fromList compareCoord)
        (Decode.list
            (Decode.tuple3 (\x y cell -> ( ( x, y ), cell ))
                Decode.int
                Decode.int
                decodeCell
            )
        )


decodeCell : Decoder Cell
decodeCell =
    Decode.string
        `Decode.andThen` (\s ->
                            case s of
                                "Head" ->
                                    Decode.succeed Head

                                "Tail" ->
                                    Decode.succeed Tail

                                "Conductor" ->
                                    Decode.succeed Conductor

                                _ ->
                                    Decode.fail "Unknown cell type"
                         )


cellFill : Maybe Cell -> String
cellFill cell =
    case cell of
        Just Head ->
            "red"

        Just Tail ->
            "orange"

        Just Conductor ->
            "yellow"

        Nothing ->
            "#333"


viewCell : Grid -> Coord -> Svg Msg
viewCell grid ( x, y ) =
    rect
        [ A.x (toString (x * 20))
        , A.y (toString (y * 20))
        , A.width "19"
        , A.height "19"
        , A.fill (cellFill (Dict.get ( x, y ) grid))
        , E.onClick (Click ( x, y ))
        ]
        []


viewGrid : Grid -> Svg Msg
viewGrid grid =
    g [ A.transform "translate(390, 290)" ]
        (allCells
            |> List.map (viewCell grid)
        )


saveButton : Svg Msg
saveButton =
    rect
        [ A.x "0"
        , A.y "0"
        , A.width "30"
        , A.height "30"
        , A.fill "pink"
        , E.onClick Save
        ]
        []


loadButton : Svg Msg
loadButton =
    rect
        [ A.x "40"
        , A.y "0"
        , A.width "30"
        , A.height "30"
        , A.fill "green"
        , E.onClick Load
        ]
        []


view : Model -> Svg Msg
view model =
    svg
        [ A.style "background-color: #000000"
        , A.width "800"
        , A.height "600"
        ]
        [ viewGrid model.grid, saveButton, loadButton ]


allCells : List Coord
allCells =
    List.concatMap (\x -> List.map (\y -> ( x, y )) [-14..14]) [-19..19]
