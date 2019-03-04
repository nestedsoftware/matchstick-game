module TestMatchsticks exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html exposing (..)
import Main exposing (..)
import Random
import Test exposing (..)


suite : Test
suite =
    describe "Matchsticks Tests"
        [ test "matchsticksToTake gives correct values for 1 to 21" <|
            \_ ->
                let
                    expectedMatchsticksToTake =
                        [ ( 1, 1 ) -- lose
                        , ( 2, 1 ) -- win
                        , ( 3, 2 ) -- win
                        , ( 4, 3 ) -- win
                        , ( 5, 1 ) -- lose
                        , ( 6, 1 ) -- win
                        , ( 7, 2 ) -- win
                        , ( 8, 3 ) -- win
                        , ( 9, 1 ) -- lose
                        , ( 10, 1 ) -- win
                        , ( 11, 2 ) -- win
                        , ( 12, 3 ) -- win
                        , ( 13, 1 ) -- lose
                        , ( 14, 1 ) -- win
                        , ( 15, 2 ) -- win
                        , ( 16, 3 ) -- win
                        , ( 17, 1 ) -- lose
                        , ( 18, 1 ) -- win
                        , ( 19, 2 ) -- win
                        , ( 20, 3 ) -- win
                        , ( 21, 1 ) -- lose
                        ]

                    matchsticks =
                        List.range 1 21

                    actualMatchsticksToTake =
                        List.map
                            (\count -> matchsticksToTake count)
                            matchsticks
                in
                Expect.equal
                    expectedMatchsticksToTake
                    actualMatchsticksToTake
        , test "update should ignore player when it is computer's turn" <|
            \_ ->
                let
                    startingModel =
                        Model ComputerPlayer 21 NoneSelected

                    ( actualModel, actualMsg ) =
                        updateWithoutCmd (Take 1) startingModel
                in
                Expect.equal
                    ( startingModel, DoNothing )
                    ( actualModel, actualMsg )
        , test "update should ignore computer when it is player's turn" <|
            \_ ->
                let
                    startingModel =
                        Model HumanPlayer 21 NoneSelected

                    ( actualModel, actualMsg ) =
                        updateWithoutCmd (ComputerTake 1) startingModel
                in
                Expect.equal
                    ( startingModel, DoNothing )
                    ( actualModel, actualMsg )
        , fuzz (Fuzz.intRange Random.minInt 0)
            "human should not be able to take less than 1 matchsticks"
          <|
            \matchsticksToTake ->
                let
                    startingModel =
                        Model HumanPlayer 21 NoneSelected

                    ( actualModel, actualMsg ) =
                        updateWithoutCmd (Take matchsticksToTake) startingModel
                in
                Expect.equal
                    ( startingModel, DoNothing )
                    ( actualModel, actualMsg )
        , fuzz (Fuzz.intRange 4 Random.maxInt)
            "human should not be able to take more than 3 matchsticks"
          <|
            \matchsticksToTake ->
                let
                    startingModel =
                        Model HumanPlayer 21 NoneSelected

                    ( actualModel, actualMsg ) =
                        updateWithoutCmd (Take matchsticksToTake) startingModel
                in
                Expect.equal
                    ( startingModel, DoNothing )
                    ( actualModel, actualMsg )
        , fuzz (Fuzz.intRange 1 3)
            "human should be able to take between 1 and 3 matchsticks"
          <|
            \matchsticksToTake ->
                let
                    startingModel =
                        Model HumanPlayer 21 NoneSelected

                    ( actualModel, actualMsg ) =
                        updateWithoutCmd (Take matchsticksToTake) startingModel

                    expectedModel =
                        Model
                            ComputerPlayer
                            (21 - matchsticksToTake)
                            (Selected HumanPlayer matchsticksToTake)

                    expectedMsg =
                        computerTakesNextTurn (21 - matchsticksToTake)
                in
                Expect.equal
                    ( expectedModel, expectedMsg )
                    ( actualModel, actualMsg )
        , fuzz (Fuzz.intRange 1 3)
            "computer should be able to take between 1 and 3 matchsticks"
          <|
            \matchsticksToTake ->
                let
                    startingModel =
                        Model ComputerPlayer 21 NoneSelected

                    ( actualModel, actualMsg ) =
                        updateWithoutCmd
                            (ComputerTake matchsticksToTake)
                            startingModel

                    expectedModel =
                        Model
                            HumanPlayer
                            (21 - matchsticksToTake)
                            (Selected ComputerPlayer matchsticksToTake)
                in
                Expect.equal
                    ( expectedModel, DoNothing )
                    ( actualModel, actualMsg )
        , fuzz (Fuzz.intRange 2 3)
            "player cannot take more matchsticks than the total available"
          <|
            \matchsticksToTake ->
                let
                    startingModel =
                        Model HumanPlayer 1 NoneSelected

                    ( actualModel, actualMsg ) =
                        updateWithoutCmd (Take matchsticksToTake) startingModel

                    expectedModel =
                        Model
                            ComputerPlayer
                            0
                            (Selected HumanPlayer matchsticksToTake)
                in
                Expect.equal
                    ( expectedModel, DoNothing )
                    ( actualModel, actualMsg )
        ]
