module Main exposing (..)

import Array exposing (..)
import Browser
import Html exposing (Html, button, div, h2, h4, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Square
    = Player Player
    | Empty


type Player
    = X
    | O


type alias Board =
    Array Square


type alias Win =
    { lines : ( Int, Int, Int )
    , winner : Player
    }


type alias Model =
    { board : Board
    , current_player : Player
    , hasWon : Maybe Win
    , history : List History
    }


type History
    = Entry Model

board_size : Int
board_size =
    3

winning_lines: List (Int, Int, Int)
winning_lines =
    [ ( 0, 1, 2 )
    , ( 3, 4, 5 )
    , ( 6, 7, 8 )
    , ( 0, 3, 6 )
    , ( 1, 4, 7 )
    , ( 2, 5, 8 )
    , ( 0, 4, 8 )
    , ( 2, 4, 6 )
    ]


init : Model
init =
    { board =
        always Empty
            |> Array.initialize (board_size * board_size)
    , current_player = X
    , hasWon = Nothing
    , history = []
    }


type Msg
    = Click Int
    | Restart
    | GoBackTo Model



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Click id ->
            handleClick id model

        Restart ->
            init

        GoBackTo history ->
            history


hasLine : Player -> Board -> ( Int, Int, Int ) -> Bool
hasLine player board line =
    let
        ( x, y, z ) =
            line

        x_line =
            Maybe.withDefault Empty (Array.get x board)

        y_line =
            Maybe.withDefault Empty (Array.get y board)

        z_line =
            Maybe.withDefault Empty (Array.get z board)

        current_lines =
            [ x_line, y_line, z_line ]

        ownedByPlayer : Square -> Bool
        ownedByPlayer square =
            case square of
                Player owner ->
                    player == owner

                _ ->
                    False
    in
    List.all ownedByPlayer current_lines


hasWon : Player -> Board -> Maybe Win
hasWon player board =
    let
        filtered_lines =
            List.filter (hasLine player board) winning_lines
    in
    case List.head filtered_lines of
        Just line ->
            Just (Win line player)

        Nothing ->
            Nothing


handleClick : Int -> Model -> Model
handleClick id model =
    let
        isEmpty =
            Maybe.withDefault Empty (Array.get id model.board)
                |> hasPlayer
                |> not

        updatedBoard =
            if isEmpty && not gameEnd then
                Array.set id (Player model.current_player) model.board

            else
                model.board

        gameEnd =
            case model.hasWon of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    if model.board /= updatedBoard then
        { model
            | board = updatedBoard
            , current_player = nextPlayer model.current_player
            , hasWon = hasWon model.current_player updatedBoard
            , history = model.history ++ [ Entry model ]
        }

    else
        model


hasPlayer : Square -> Bool
hasPlayer square =
    case square of
        Player _ ->
            True

        Empty ->
            False


nextPlayer : Player -> Player
nextPlayer player =
    case player of
        X ->
            O

        O ->
            X



-- VIEW


toStringPlayer : Player -> String
toStringPlayer player =
    case player of
        X ->
            "X"

        O ->
            "O"


viewSquare : Model -> ( Int, Square ) -> Html Msg
viewSquare model square =
    let
        ( id, owner ) =
            square

        squareRowClass =
            if modBy board_size id == 0 then
                "first"

            else
                ""

        squareRedClass =
            if isWinningSquare then
                "red"

            else
                ""

        squareClass =
            "square " ++ squareRowClass ++ " " ++ squareRedClass


        winningSquares =
            case model.hasWon of
                Just win ->
                    win.lines

                Nothing ->
                    ( -1, -1, -1 )

        isWinningSquare =
            let
                ( x, y, z ) =
                    winningSquares
            in
            id == x || id == y || id == z

        playerText =
            case owner of
                Player player ->
                    toStringPlayer player

                Empty ->
                    ""
    in
    button [ class squareClass, onClick (Click id) ] [ text playerText ]


viewBoard : Model -> Html Msg
viewBoard model =
    Array.toIndexedList model.board
        |> List.map (viewSquare model)
        |> div [ class "" ]


viewGameInfo : Model -> Html Msg
viewGameInfo model =
    let
        game_text =
            case model.hasWon of
                Just win ->
                    "Player " ++ toStringPlayer win.winner ++ " has won!"

                Nothing ->
                    "Current Player: " ++ toStringPlayer model.current_player
    in
    div
        [ class "game-info" ]
        [ h4 [] [ text game_text ]
        , button [ onClick Restart ] [ text "Restart" ]
        ]


viewHistory : List History -> Html Msg
viewHistory history =
    let
        viewHistoryEntry : History -> Html Msg
        viewHistoryEntry entry =
            let
                ent =
                    case entry of
                        Entry cur ->
                            cur

                playerText =
                    toStringPlayer ent.current_player

                round =
                    List.length ent.history
                        |> (+) 1
                        |> String.fromInt

                history_text =
                    "Redo move #" ++ round ++ " by Player " ++ playerText
            in
            button [ class "history", onClick (GoBackTo ent) ] [ text history_text ]
    in
    div [ class "game-info" ] (List.map viewHistoryEntry history)


view : Model -> Html Msg
view model =
    div []
        [ h2 []
            [ text "Tic-Tac-Toe Game" ]
        , div [ class "row" ]
            [ div [ class "col-6" ]
                [ viewBoard model
                ]
            ]
        , viewGameInfo model
        , viewHistory model.history
        ]
