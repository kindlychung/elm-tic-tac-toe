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


type Party
    = Player
    | Computer


type Piece
    = Occupied Party
    | Unoccupied


type Winner
    = Winner Party
    | Draw
    | Unknown


type alias Model =
    { pieces : Array Piece
    , nextTurn : Party
    , winner : Winner
    }


fourUnoccupied =
    Array.repeat 4 Unoccupied


init : Int -> ( Model, Cmd msg )
init turnInt =
    case turnInt of
        0 ->
            ( { pieces = Array.append (Array.append fourUnoccupied (Array.fromList [ Occupied Computer ])) fourUnoccupied, nextTurn = Player, winner = Unknown }, Cmd.none )

        _ ->
            ( { pieces = Array.repeat 9 Unoccupied, nextTurn = Player, winner = Unknown }, Cmd.none )


playerPiece =
    "x"


computerPiece =
    "o"


symbol : Piece -> String
symbol turn =
    case turn of
        Occupied Player ->
            playerPiece

        Occupied Computer ->
            computerPiece

        Unoccupied ->
            " "


getLineIndexed : Int -> Int -> Int -> Array Piece -> Array ( Int, Piece )
getLineIndexed i j k turns =
    Array.Extra.zip
        (Array.fromList [ i, j, k ])
        (Array.fromList (Maybe.Extra.values [ Array.get i turns, Array.get j turns, Array.get k turns ]))


getLine : Int -> Int -> Int -> Array Piece -> Array Piece
getLine i j k turns =
    Array.fromList (Maybe.Extra.values [ Array.get i turns, Array.get j turns, Array.get k turns ])


getRow1Indexed : Array Piece -> Array ( Int, Piece )
getRow1Indexed turns =
    getLineIndexed 0 1 2 turns


getRow2Indexed : Array Piece -> Array ( Int, Piece )
getRow2Indexed turns =
    getLineIndexed 3 4 5 turns


getRow3Indexed : Array Piece -> Array ( Int, Piece )
getRow3Indexed turns =
    getLineIndexed 6 7 8 turns


getCol1Indexed : Array Piece -> Array ( Int, Piece )
getCol1Indexed turns =
    getLineIndexed 0 3 6 turns


getCol2Indexed : Array Piece -> Array ( Int, Piece )
getCol2Indexed turns =
    getLineIndexed 1 4 7 turns


getCol3Indexed : Array Piece -> Array ( Int, Piece )
getCol3Indexed turns =
    getLineIndexed 2 5 8 turns


getDiag1Indexed : Array Piece -> Array ( Int, Piece )
getDiag1Indexed turns =
    getLineIndexed 0 4 8 turns


getDiag2Indexed : Array Piece -> Array ( Int, Piece )
getDiag2Indexed turns =
    getLineIndexed 2 4 6 turns


getRow1 : Array Piece -> Array Piece
getRow1 turns =
    Array.slice 0 3 turns


getRow2 : Array Piece -> Array Piece
getRow2 turns =
    Array.slice 3 6 turns


getRow3 : Array Piece -> Array Piece
getRow3 turns =
    Array.slice 6 9 turns


getCol1 : Array Piece -> Array Piece
getCol1 turns =
    getLine 0 3 6 turns


getCol2 : Array Piece -> Array Piece
getCol2 turns =
    getLine 1 4 7 turns


getCol3 : Array Piece -> Array Piece
getCol3 turns =
    getLine 2 5 8 turns


getDiag1 : Array Piece -> Array Piece
getDiag1 turns =
    getLine 0 4 8 turns


getDiag2 : Array Piece -> Array Piece
getDiag2 turns =
    getLine 2 4 6 turns


winningLine : Piece -> List Piece -> Bool
winningLine currentTurn turns =
    List.all (\x -> x == currentTurn) turns


hasWon : Piece -> Model -> Bool
hasWon currentTurn model =
    winningLine currentTurn (Array.toList (getRow1 model.pieces))
        || winningLine currentTurn (Array.toList (getRow2 model.pieces))
        || winningLine currentTurn (Array.toList (getRow3 model.pieces))
        || winningLine currentTurn (Array.toList (getCol1 model.pieces))
        || winningLine currentTurn (Array.toList (getCol2 model.pieces))
        || winningLine currentTurn (Array.toList (getCol3 model.pieces))
        || winningLine currentTurn (Array.toList (getDiag1 model.pieces))
        || winningLine currentTurn (Array.toList (getDiag2 model.pieces))


getKillingPosFromLine : Piece -> List ( Int, Piece ) -> Maybe Int
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


getKillingPos : Piece -> Model -> Maybe Int
getKillingPos piece model =
    getKillingPosFromLine piece (Array.toList (getRow1Indexed model.pieces))
        |> Maybe.Extra.orElseLazy
            (\() -> getKillingPosFromLine piece (Array.toList (getRow2Indexed model.pieces)))
        |> Maybe.Extra.orElseLazy
            (\() -> getKillingPosFromLine piece (Array.toList (getRow3Indexed model.pieces)))
        |> Maybe.Extra.orElseLazy
            (\() -> getKillingPosFromLine piece (Array.toList (getCol1Indexed model.pieces)))
        |> Maybe.Extra.orElseLazy
            (\() -> getKillingPosFromLine piece (Array.toList (getCol2Indexed model.pieces)))
        |> Maybe.Extra.orElseLazy
            (\() -> getKillingPosFromLine piece (Array.toList (getCol3Indexed model.pieces)))
        |> Maybe.Extra.orElseLazy
            (\() -> getKillingPosFromLine piece (Array.toList (getDiag1Indexed model.pieces)))
        |> Maybe.Extra.orElseLazy
            (\() -> getKillingPosFromLine piece (Array.toList (getDiag2Indexed model.pieces)))


getUnoccupiedIndex : Int -> Array Piece -> Maybe Int
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
    if hasWon (Occupied Player) model then
        Winner Player

    else if hasWon (Occupied Computer) model then
        Winner Computer

    else if noStepLeft model then
        Draw

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
                            { model | nextTurn = Computer, pieces = Array.set position (Occupied Player) model.pieces }

                        newModel1 =
                            { newModel
                                | winner = getWinner newModel
                            }
                    in
                    ( newModel1
                    , if newModel1.winner == Winner Player then
                        Cmd.none

                      else
                        perform ComputerPut
                    )

                Computer ->
                    ( model, perform ComputerPut )

        ComputerPut ->
            case getKillingPos (Occupied Computer) model of
                -- if there is a killing move, do it
                Just idx ->
                    ( { model | pieces = Array.set idx (Occupied Computer) model.pieces, nextTurn = Player, winner = Winner Computer }, Cmd.none )

                Nothing ->
                    -- if there is a killing move for human, block it
                    case getKillingPos (Occupied Player) model of
                        Just idx ->
                            ( { model | pieces = Array.set idx (Occupied Computer) model.pieces, nextTurn = Player }, Cmd.none )

                        Nothing ->
                            -- if the center is no occupied, occupy it
                            if Array.get 4 model.pieces == Just Unoccupied then
                                ( { model | pieces = Array.set 4 (Occupied Computer) model.pieces, nextTurn = Player }, Cmd.none )

                            else
                                case getUnoccupiedCorner model of
                                    -- if there is a corner to take, take it
                                    Just idx ->
                                        ( { model | pieces = Array.set idx (Occupied Computer) model.pieces, nextTurn = Player }, Cmd.none )

                                    Nothing ->
                                        case getUnoccupiedEdge model of
                                            Just idx ->
                                                ( { model | pieces = Array.set idx (Occupied Computer) model.pieces, nextTurn = Player }, Cmd.none )

                                            Nothing ->
                                                ( model, Cmd.none )



-- VIEW


pieceHolder : Int -> Piece -> Element Msg
pieceHolder idx piece =
    el
        [ Background.color (rgb255 150 90 85)
        , width (px 90)
        , Font.color (rgb255 255 255 255)
        , Font.family [ Font.monospace ]
        , Border.rounded 5
        , padding 30
        , onClick (PlayerPut idx)
        ]
        (el [ centerX, centerY ] (text (symbol piece)))


piecesRow : Int -> Array Piece -> Element Msg
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
                    Winner Player ->
                        "Human won!"

                    Winner Computer ->
                        "Computer won!"

                    Draw ->
                        "It's a draw!"

                    Unknown ->
                        "..."
                )
            )
        ]


view : Model -> Html Msg
view model =
    Element.layout [] (board model)
