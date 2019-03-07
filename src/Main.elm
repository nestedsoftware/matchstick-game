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
            [ text <| playerTurnMessage model.matchsticks model.currentPlayer ]
        , h1 [] [ text (String.fromInt model.matchsticks) ]
        , button [ onClick (Take 1), disabled (disable model) ]
            [ text "take 1" ]
        , button [ onClick (Take 2), disabled (disable model) ]
            [ text "take 2" ]
        , button [ onClick (Take 3), disabled (disable model) ]
            [ text "take 3" ]
        , displayLastMoveMessageAndThinkingStatus model
        ]


displayLastMoveMessageAndThinkingStatus : Model -> Html Msg
displayLastMoveMessageAndThinkingStatus model =
    let
        lastMove =
            lastMoveMessage model.lastSelection

        moveMessage =
            span [ style "font-size" "1.5em" ]
                [ text <| lastMove ]

        thinkingMessage =
            let
                visibility =
                    if
                        model.currentPlayer
                            == ComputerPlayer
                            && not (gameOver model.matchsticks)
                    then
                        "visible"

                    else
                        "hidden"
            in
            displayComputerThinkingMessage
                model.currentPlayer
                lastMove
                visibility
    in
    div []
        [ moveMessage
        , thinkingMessage
        ]


displayComputerThinkingMessage : Player -> String -> String -> Html Msg
displayComputerThinkingMessage currentPlayer lastMove visibility =
    let
        commaIfNeeded =
            if lastMove == "" then
                ""

            else
                ", "
    in
    span [ style "visibility" visibility, style "font-size" "1.5em" ]
        [ p [] []
        , span [ class "saving" ]
            [ text <|
                commaIfNeeded
                    ++ playerLabel currentPlayer
                    ++ " is thinking"
            , span [] [ text "." ]
            , span [] [ text "." ]
            , span [] [ text "." ]
            ]
        ]


playerTurnMessage : Int -> Player -> String
playerTurnMessage matchsticks player =
    if matchsticks == 0 then
        "Game over, " ++ playerLabel player ++ " won!"

    else
        "It is " ++ possessive player ++ " turn"


playerLabel : Player -> String
playerLabel currentPlayer =
    case currentPlayer of
        ComputerPlayer ->
            "computer"

        HumanPlayer ->
            "you"


lastMoveMessage : Selection -> String
lastMoveMessage selection =
    case selection of
        NoneSelected ->
            ""

        Selected player matchsticks ->
            possessive player
                ++ " selection was: "
                ++ String.fromInt matchsticks


possessive : Player -> String
possessive player =
    case player of
        ComputerPlayer ->
            "computer's"

        HumanPlayer ->
            "your"


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
