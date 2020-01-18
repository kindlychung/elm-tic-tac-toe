module Main exposing (..)

import Array exposing (Array)
import Array.Extra
import Browser
import Cmd.Extra exposing (perform)
import Element exposing (Element, centerX, centerY, column, el, height, padding, px, rgb255, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Html exposing (Html)
import Maybe exposing (Maybe)
import Maybe.Extra



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type Turn
    = Player
    | Computer
    | Unoccupied


type Winner
    = PlayerW
    | ComputerW
    | None
    | Unknown


type alias Model =
    { pieces : Array Turn
    , nextTurn : Turn
    , winner : Winner
    }


fourUnoccupied =
    Array.repeat 4 Unoccupied


init : Int -> ( Model, Cmd msg )
init turnInt =
    case turnInt of
        0 ->
            ( { pieces = Array.append (Array.append fourUnoccupied (Array.fromList [ Computer ])) fourUnoccupied, nextTurn = Player, winner = Unknown }, Cmd.none )

        _ ->
            ( { pieces = Array.repeat 9 Unoccupied, nextTurn = Player, winner = Unknown }, Cmd.none )


playerPiece =
    "x"


computerPiece =
    "o"


symbol : Turn -> String
symbol turn =
    case turn of
        Player ->
            playerPiece

        Computer ->
            computerPiece

        Unoccupied ->
            " "


getLineIndexed : Int -> Int -> Int -> Array Turn -> Array ( Int, Turn )
getLineIndexed i j k turns =
    Array.Extra.zip
        (Array.fromList [ i, j, k ])
        (Array.fromList (Maybe.Extra.values [ Array.get i turns, Array.get j turns, Array.get k turns ]))


getLine : Int -> Int -> Int -> Array Turn -> Array Turn
getLine i j k turns =
    Array.fromList (Maybe.Extra.values [ Array.get i turns, Array.get j turns, Array.get k turns ])


getRow1Indexed : Array Turn -> Array ( Int, Turn )
getRow1Indexed turns =
    getLineIndexed 0 1 2 turns


getRow2Indexed : Array Turn -> Array ( Int, Turn )
getRow2Indexed turns =
    getLineIndexed 3 4 5 turns


getRow3Indexed : Array Turn -> Array ( Int, Turn )
getRow3Indexed turns =
    getLineIndexed 6 7 8 turns


getCol1Indexed : Array Turn -> Array ( Int, Turn )
getCol1Indexed turns =
    getLineIndexed 0 3 6 turns


getCol2Indexed : Array Turn -> Array ( Int, Turn )
getCol2Indexed turns =
    getLineIndexed 1 4 7 turns


getCol3Indexed : Array Turn -> Array ( Int, Turn )
getCol3Indexed turns =
    getLineIndexed 2 5 8 turns


getDiag1Indexed : Array Turn -> Array ( Int, Turn )
getDiag1Indexed turns =
    getLineIndexed 0 4 8 turns


getDiag2Indexed : Array Turn -> Array ( Int, Turn )
getDiag2Indexed turns =
    getLineIndexed 2 4 6 turns


getRow1 : Array Turn -> Array Turn
getRow1 turns =
    Array.slice 0 3 turns


getRow2 : Array Turn -> Array Turn
getRow2 turns =
    Array.slice 3 6 turns


getRow3 : Array Turn -> Array Turn
getRow3 turns =
    Array.slice 6 9 turns


getCol1 : Array Turn -> Array Turn
getCol1 turns =
    getLine 0 3 6 turns


getCol2 : Array Turn -> Array Turn
getCol2 turns =
    getLine 1 4 7 turns


getCol3 : Array Turn -> Array Turn
getCol3 turns =
    getLine 2 5 8 turns


getDiag1 : Array Turn -> Array Turn
getDiag1 turns =
    getLine 0 4 8 turns


getDiag2 : Array Turn -> Array Turn
getDiag2 turns =
    getLine 2 4 6 turns


winningLine : Turn -> List Turn -> Bool
winningLine currentTurn turns =
    List.all (\x -> x == currentTurn) turns


hasWon : Turn -> Model -> Bool
hasWon currentTurn model =
    winningLine currentTurn (Array.toList (getRow1 model.pieces))
        || winningLine currentTurn (Array.toList (getRow2 model.pieces))
        || winningLine currentTurn (Array.toList (getRow3 model.pieces))
        || winningLine currentTurn (Array.toList (getCol1 model.pieces))
        || winningLine currentTurn (Array.toList (getCol2 model.pieces))
        || winningLine currentTurn (Array.toList (getCol3 model.pieces))
        || winningLine currentTurn (Array.toList (getDiag1 model.pieces))
        || winningLine currentTurn (Array.toList (getDiag2 model.pieces))


getKillingPosFromLine : Turn -> List ( Int, Turn ) -> Maybe Int
getKillingPosFromLine currentTurn turns =
    let
        ( sameTurns, diffTurns ) =
            List.partition (\( _, x ) -> x == currentTurn) turns
    in
    case List.head diffTurns of
        -- this is true when one side has already won, will not happen in this function
        Nothing ->
            Nothing

        Just diffElem ->
            if (List.length sameTurns == 2) && Tuple.second diffElem == Unoccupied then
                Just (Tuple.first diffElem)

            else
                Nothing


getKillingPos : Turn -> Model -> Maybe Int
getKillingPos turn model =
    getKillingPosFromLine turn (Array.toList (getRow1Indexed model.pieces))
        |> Maybe.Extra.orElseLazy
            (\() -> getKillingPosFromLine turn (Array.toList (getRow2Indexed model.pieces)))
        |> Maybe.Extra.orElseLazy
            (\() -> getKillingPosFromLine turn (Array.toList (getRow3Indexed model.pieces)))
        |> Maybe.Extra.orElseLazy
            (\() -> getKillingPosFromLine turn (Array.toList (getCol1Indexed model.pieces)))
        |> Maybe.Extra.orElseLazy
            (\() -> getKillingPosFromLine turn (Array.toList (getCol2Indexed model.pieces)))
        |> Maybe.Extra.orElseLazy
            (\() -> getKillingPosFromLine turn (Array.toList (getCol3Indexed model.pieces)))
        |> Maybe.Extra.orElseLazy
            (\() -> getKillingPosFromLine turn (Array.toList (getDiag1Indexed model.pieces)))
        |> Maybe.Extra.orElseLazy
            (\() -> getKillingPosFromLine turn (Array.toList (getDiag2Indexed model.pieces)))


getUnoccupiedIndex : Int -> Array Turn -> Maybe Int
getUnoccupiedIndex idx turns =
    Maybe.andThen
        (\x ->
            if x == Unoccupied then
                Just idx

            else
                Nothing
        )
        (Array.get idx turns)


getUnoccupiedCorner : Model -> Maybe Int
getUnoccupiedCorner model =
    getUnoccupiedIndex 0 model.pieces
        |> Maybe.Extra.orElseLazy
            (\() -> getUnoccupiedIndex 2 model.pieces)
        |> Maybe.Extra.orElseLazy
            (\() -> getUnoccupiedIndex 6 model.pieces)
        |> Maybe.Extra.orElseLazy
            (\() -> getUnoccupiedIndex 8 model.pieces)


getUnoccupiedEdge : Model -> Maybe Int
getUnoccupiedEdge model =
    getUnoccupiedIndex 1 model.pieces
        |> Maybe.Extra.orElseLazy
            (\() -> getUnoccupiedIndex 3 model.pieces)
        |> Maybe.Extra.orElseLazy
            (\() -> getUnoccupiedIndex 5 model.pieces)
        |> Maybe.Extra.orElseLazy
            (\() -> getUnoccupiedIndex 7 model.pieces)


noStepLeft : Model -> Bool
noStepLeft model =
    List.all (\x -> x /= Unoccupied) (Array.toList model.pieces)



-- UPDATE


type Msg
    = PlayerPut Int
    | ComputerPut


getWinner : Model -> Winner
getWinner model =
    if hasWon Player model then
        PlayerW

    else if hasWon Computer model then
        ComputerW

    else if noStepLeft model then
        None

    else
        Unknown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayerPut position ->
            case model.nextTurn of
                Player ->
                    let
                        newModel =
                            { model | nextTurn = Computer, pieces = Array.set position Player model.pieces }

                        newModel1 =
                            { newModel
                                | winner = getWinner newModel
                            }
                    in
                    ( newModel1
                    , if newModel1.winner == PlayerW then
                        Cmd.none

                      else
                        perform ComputerPut
                    )

                Computer ->
                    ( model, perform ComputerPut )

                Unoccupied ->
                    ( model, Cmd.none )

        ComputerPut ->
            case getKillingPos Computer model of
                -- if there is a killing move, do it
                Just idx ->
                    ( { model | pieces = Array.set idx Computer model.pieces, nextTurn = Player, winner = ComputerW }, Cmd.none )

                Nothing ->
                    -- if there is a killing move for human, block it
                    case getKillingPos Player model of
                        Just idx ->
                            ( { model | pieces = Array.set idx Computer model.pieces, nextTurn = Player }, Cmd.none )

                        Nothing ->
                            -- if the center is no occupied, occupy it
                            if Array.get 4 model.pieces == Just Unoccupied then
                                ( { model | pieces = Array.set 4 Computer model.pieces, nextTurn = Player }, Cmd.none )

                            else
                                case getUnoccupiedCorner model of
                                    -- if there is a corner to take, take it
                                    Just idx ->
                                        ( { model | pieces = Array.set idx Computer model.pieces, nextTurn = Player }, Cmd.none )

                                    Nothing ->
                                        case getUnoccupiedEdge model of
                                            Just idx ->
                                                ( { model | pieces = Array.set idx Computer model.pieces, nextTurn = Player }, Cmd.none )

                                            Nothing ->
                                                ( model, Cmd.none )



-- VIEW


pieceHolder : Int -> Turn -> Element Msg
pieceHolder idx turn =
    el
        [ Background.color (rgb255 150 90 85)
        , width (px 90)
        , Font.color (rgb255 255 255 255)
        , Font.family [ Font.monospace ]
        , Border.rounded 5
        , padding 30
        , onClick (PlayerPut idx)
        ]
        (el [ centerX, centerY ] (text (symbol turn)))


piecesRow : Int -> Array Turn -> Element Msg
piecesRow rowIdx turns =
    row [ width shrink, spacing 30 ]
        (Array.toList (Array.Extra.map2 pieceHolder (Array.fromList (List.range (rowIdx * 3) (rowIdx * 3 + 3))) turns))


board : Model -> Element Msg
board model =
    column
        [ height shrink, centerY, centerX, spacing 30 ]
        [ piecesRow 0 (getRow1 model.pieces)
        , piecesRow 1 (getRow2 model.pieces)
        , piecesRow 2 (getRow3 model.pieces)
        , el
            [ centerX
            , Font.family [ Font.typeface "Amarante", Font.sansSerif ]
            , Font.size 45
            ]
            (text
                (case model.winner of
                    PlayerW ->
                        "Human won!"

                    ComputerW ->
                        "Computer won!"

                    None ->
                        "It's a draw!"

                    Unknown ->
                        "..."
                )
            )
        ]


view : Model -> Html Msg
view model =
    Element.layout [] (board model)
