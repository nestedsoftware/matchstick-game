module Main exposing
    ( Model
    , Msg(..)
    , Player(..)
    , Selection(..)
    , computerTakesNextTurn
    , init
    , main
    , matchsticksToTake
    , subscriptions
    , update
    , updateWithoutCmd
    , view
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, disabled, style)
import Html.Events exposing (..)
import Process
import Task



-- Business Logic


multipleAndRemainder : Int -> ( Int, Int, Int )
multipleAndRemainder count =
    let
        multiple =
            (count - 1) // 4

        remainder =
            remainderBy 4 (count - 1)
    in
    ( count, multiple, remainder )


targetCount : Int -> Int
targetCount multiple =
    multiple * 4 + 1


matchsticksToTake : Int -> ( Int, Int )
matchsticksToTake count =
    let
        ( _, multiple, remainder ) =
            multipleAndRemainder count

        toTake =
            if remainder == 0 then
                ( count, 1 )

            else
                ( count, count - targetCount multiple )
    in
    toTake



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { currentPlayer : Player
    , matchsticks : Int
    , lastSelection : Selection
    }


type Player
    = ComputerPlayer
    | HumanPlayer


type Selection
    = Selected Player Int
    | NoneSelected


nextPlayer : Player -> Player
nextPlayer currentPlayer =
    case currentPlayer of
        ComputerPlayer ->
            HumanPlayer

        HumanPlayer ->
            ComputerPlayer


init : () -> ( Model, Cmd Msg )
init _ =
    -- let the computer start the game
    wrapNextMsgWithCmd
        ( Model ComputerPlayer 21 NoneSelected, computerTakesNextTurn 21 )



-- UPDATE


type Msg
    = ComputerTake Int
    | Take Int
    | DoNothing


{-| current msg -> current model -> (new model, message received from cmd)
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    wrapNextMsgWithCmd <| updateWithoutCmd msg model


wrapNextMsgWithCmd : ( Model, Msg ) -> ( Model, Cmd Msg )
wrapNextMsgWithCmd ( nextModel, nextMsg ) =
    ( nextModel, wrapWithCmd nextMsg )


wrapWithCmd : Msg -> Cmd Msg
wrapWithCmd nextMsg =
    case nextMsg of
        DoNothing ->
            Cmd.none

        Take _ ->
            Cmd.none

        ComputerTake _ ->
            Cmd.batch
                [ Task.perform
                    (\_ -> nextMsg)
                    (Process.sleep 3000)
                ]


updateWithoutCmd : Msg -> Model -> ( Model, Msg )
updateWithoutCmd msg model =
    case msg of
        Take selectedMatchsticks ->
            humanPlayerTakesTurn model selectedMatchsticks

        ComputerTake selectedMatchsticks ->
            computerPlayerTakesTurn model selectedMatchsticks

        DoNothing ->
            ( model, DoNothing )


humanPlayerTakesTurn : Model -> Int -> ( Model, Msg )
humanPlayerTakesTurn model selectedMatchsticks =
    case model.currentPlayer of
        HumanPlayer ->
            tryToPlayTurn
                model
                selectedMatchsticks
                (computerPlaysNextOrEndOfGame
                    model.matchsticks
                    selectedMatchsticks
                )

        ComputerPlayer ->
            rejectPlayerTurn model


computerPlayerTakesTurn : Model -> Int -> ( Model, Msg )
computerPlayerTakesTurn model selectedMatchsticks =
    case model.currentPlayer of
        ComputerPlayer ->
            tryToPlayTurn model selectedMatchsticks DoNothing

        HumanPlayer ->
            rejectPlayerTurn model


tryToPlayTurn : Model -> Int -> Msg -> ( Model, Msg )
tryToPlayTurn model selectedMatchsticks msg =
    if selectedMatchsticks < 1 || selectedMatchsticks > 3 then
        ( model, DoNothing )

    else
        ( updateSelection model selectedMatchsticks, msg )


updateSelection : Model -> Int -> Model
updateSelection model selectedMatchsticks =
    if gameOver model.matchsticks then
        model

    else
        { model
            | currentPlayer = nextPlayer model.currentPlayer
            , matchsticks =
                takeMatchsticks model.matchsticks selectedMatchsticks
            , lastSelection = Selected model.currentPlayer selectedMatchsticks
        }


rejectPlayerTurn : Model -> ( Model, Msg )
rejectPlayerTurn model =
    ( model, DoNothing )


computerPlaysNextOrEndOfGame : Int -> Int -> Msg
computerPlaysNextOrEndOfGame matchsticks selectedMatchsticks =
    if gameOver (matchsticks - selectedMatchsticks) then
        DoNothing

    else
        computerTakesNextTurn <|
            takeMatchsticks matchsticks selectedMatchsticks


gameOver : Int -> Bool
gameOver matchsticks =
    if matchsticks > 0 then
        False

    else
        True


computerTakesNextTurn : Int -> Msg
computerTakesNextTurn matchsticks =
    ComputerTake <| Tuple.second <| matchsticksToTake matchsticks


takeMatchsticks : Int -> Int -> Int
takeMatchsticks matchsticks toTake =
    if matchsticks >= toTake then
        matchsticks - toTake

    else
        0



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1
            []
            [ playerTurnLabel model ]
        , h1 [] [ text (String.fromInt model.matchsticks) ]
        , button [ onClick (Take 1), disabled (disable model) ]
            [ text "take 1" ]
        , button [ onClick (Take 2), disabled (disable model) ]
            [ text "take 2" ]
        , button [ onClick (Take 3), disabled (disable model) ]
            [ text "take 3" ]
        , p [] [ text <| lastMoveString model.lastSelection ]
        ]


playerTurnLabel : Model -> Html Msg
playerTurnLabel model =
    if model.currentPlayer == HumanPlayer || gameOver model.matchsticks then
        text <| playerTurnString model.matchsticks model.currentPlayer

    else
        computerIsThinkingLabel model


playerTurnString : Int -> Player -> String
playerTurnString matchsticks player =
    if matchsticks == 0 then
        "Game over, " ++ playerLabel player ++ " won!"

    else
        "It is " ++ possessive player ++ " turn"


computerIsThinkingLabel : Model -> Html Msg
computerIsThinkingLabel model =
    span [ class "saving" ]
        [ text <|
            capitalizeFirst (playerLabel model.currentPlayer)
                ++ " is thinking"
        , span [] [ text "." ]
        , span [] [ text "." ]
        , span [] [ text "." ]
        ]


playerLabel : Player -> String
playerLabel currentPlayer =
    case currentPlayer of
        ComputerPlayer ->
            "computer"

        HumanPlayer ->
            "you"


possessive : Player -> String
possessive player =
    case player of
        ComputerPlayer ->
            playerLabel player ++ "'s"

        HumanPlayer ->
            playerLabel player ++ "r"


disable : Model -> Bool
disable model =
    case model.currentPlayer of
        ComputerPlayer ->
            True

        HumanPlayer ->
            if gameOver model.matchsticks then
                True

            else
                False


lastMoveString : Selection -> String
lastMoveString selection =
    case selection of
        NoneSelected ->
            ""

        Selected player matchsticks ->
            capitalizeFirst (possessive player)
                ++ " selection was: "
                ++ String.fromInt matchsticks


capitalizeFirst : String -> String
capitalizeFirst str =
    case String.uncons str of
        Just ( x, xs ) ->
            String.cons (Char.toUpper x) xs

        Nothing ->
            str
