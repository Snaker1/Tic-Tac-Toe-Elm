module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Array exposing (..)
import Browser
import Html exposing (Html, button, div, h2, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Square
    = X
    | O
    | Nothing


type alias Board =
    Array Square


type alias Model =
    { board : Board
    }


board_size =
    3


init : Model
init =
    { board =
        always Nothing
            |> Array.initialize (board_size * board_size)
    }


type Msg
    = Click Int



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Click id ->
            { model | board = Array.set id X model.board }


viewSquare : ( Int, Square ) -> Html Msg
viewSquare square =
    let
        player =
            case square of
                ( _, X ) ->
                    text "X"

                ( _, O ) ->
                    text "O"

                ( _, Nothing ) ->
                    text ""
    in
    div [ class "square col-4", onClick (Click (Tuple.first square)) ] [ player ]


viewBoard : Board -> Html Msg
viewBoard board =
    div [ class "row" ] (List.map viewSquare (Array.toIndexedList board))


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h2 [] [ text "Tic-Tac-Toe Game" ]
        , viewBoard model.board
        ]
