module Main where

import Data.Array
import Game
import Logic
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
    defaultMain $ tests [1,2,3]
    print ""

tests :: [Double] -> TestTree
tests dataset = testGroup "Testes da Atividade 6" 
        [ playerTest
        , peçasIniciaisTest
        , initialGameBoardTest
        , initialGameTest
        , isCoordCorrectTest
        , inversePlayerTest
        , nextPlayerTest
        , possibleMovesWhiteTest
        , possibleMovesBlackTest
        , playerTurnTest
        , moveRockBlackTest
        , moveRockWhiteTest
        , gameStateAfterMoveTest]

gameStateAfterMoveTest = testGroup "gameStateAfterMove" 
            [ testCase "Test 1 - gameStateAfterMove otherwise" (assertEqual "Test 1" Running (gameStateAfterMove initialGame))
            , testCase "Test 2 - gameStateAfterMove GameOver Just White" (assertEqual "Test 1" (GameOver (Just White)) (gameStateAfterMove tableJustWhite))
            , testCase "Test 3 - gameStateAfterMove GameOver Just Black" (assertEqual "Test 1" (GameOver (Just Black)) (gameStateAfterMove tableJustBlack))
            , testCase "Test 4 - gameStateAfterMove GameOver Nothing" (assertEqual "Test 1" (GameOver Nothing) (gameStateAfterMove tableOneWhiteOneBlack))
            ]

moveRockBlackTest = testGroup "moveRockBlack" 
            [ testCase "Test 1 - tabuleiro inicial" (assertEqual "Test 1" initialGame (moveRockBlack initialGame (9,9)))
            , testCase "Test 2 - tabuleiro inicial" (assertEqual "Test 1" initialGame (moveRockBlack initialGame (9,9)))
            , testCase "Test 3 - tabuleiro inicial" (assertEqual "Test 1" initialGame (moveRockBlack initialGame (9,9)))
            , testCase "Test 4 - tabuleiro inicial" (assertEqual "Test 1" initialGame (moveRockBlack initialGame (9,9)))
            , testCase "Test 5 - tabuleiro inicial" (assertEqual "Test 1" initialGame (moveRockBlack initialGame (9,9)))
            ]

moveRockWhiteTest = testGroup "moveRockWhite" 
            [testCase "Test 1 - tabuleiro inicial" (assertEqual "Test 1" initialGame (moveRockWhite initialGame (9,9)))
            ]

playerTurnTest = testGroup "playerTurn" 
            [ testCase "Test 1 - Click em 9,9" (assertEqual "Test 1" playerTurnInvalid (playerTurn initialGameMock (9,9)))
            , testCase "Test 1 - Click em 3,3" (assertEqual "Test 1" playerTurnInvalid (playerTurn initialGameMock (3,3)))
            , testCase "Test 1 - Click em 2,2" (assertEqual "Test 1" playerTurn22 (playerTurn initialGameMock (2,2)))
            , testCase "Test 1 - Click em 5,7" (assertEqual "Test 1" playerTurnInvalid (playerTurn initialGameMock (5,7)))
            ]

possibleMovesBlackTest = testGroup "possibleMovesBlack" 
            [testCase "Test 1 - tabuleiro inicial" (assertEqual "Test 1" initialGame (possibleMovesBlack initialGame (9,9)))
            ]

possibleMovesWhiteTest = testGroup "possibleMovesWhite" 
            [ testCase "Test 1 - tabuleiro inicial" (assertEqual "Test 1" initialGame (possibleMovesWhite initialGame (9,9)))
            , testCase "Test 2 - Click na pedra em 2,2" (assertEqual "Test 1" afterPossibleMoves22 (possibleMovesWhite initialGame (2,2)))
            ]

nextPlayerTest = testGroup "nextPlayer" 
            [testCase "Test 1 - tabuleiro inicial" (assertEqual "Test 1" nextPlayerInitialResponse (nextPlayer initialGame (0,0) (1,1) (0,0)))
            ]

initialGameTest = testGroup "initialGame" 
            [testCase "Test 1 - tabuleiro inicial" (assertEqual "Test 1" initialGameMock initialGame)
            ]

initialGameBoardTest = testGroup "initialGameBoard" 
            [testCase "Test 1 - tabuleiro" (assertEqual "Test 1" tabuleiro initialGameBoard)
            ]

peçasIniciaisTest = testGroup "peçasIniciais" 
            [testCase "Test 1 - peças inicias" (assertEqual "Test 1" peças peçasIniciais)
            ]

playerTest = testGroup "playerTest" 
            [testCase "Test 1 - Player na posição inicial correta" (assertEqual "Test 1" (Full White) (player (0,0))),
            testCase "Test 2 - Player na posição inicial correta" (assertEqual "Test 2" (Full White) (player (2,0))),
            testCase "Test 3 - Player na posição inicial correta" (assertEqual "Test 3" (Full White) (player (1,1))),
            testCase "Test 4 - Player na posição inicial correta" (assertEqual "Test 4" (Full White) (player (2,2))),
            testCase "Test 5 - Player na posição inicial correta" (assertEqual "Test 5" (Full White) (player (1,7))),
            testCase "Test 6 - Player na posição inicial correta" (assertEqual "Test 6" (Full Black) (player (7,7))),
            testCase "Test 7 - Player na posição inicial correta" (assertEqual "Test 7" (Full Black) (player (6,6))),
            testCase "Test 8 - Player na posição inicial correta" (assertEqual "Test 8" (Full Black) (player (5,5))),
            testCase "Test 9 - Player na posição inicial correta" (assertEqual "Test 9" (Full Black) (player (7,3))),
            testCase "Test 10 - Player na posição inicial correta" (assertEqual "Test 10" (Full Black) (player (7,1))),
            testCase "Test 11 - Player na posição inicial correta" (assertEqual "Test 11" Empty (player (4,4))),
            testCase "Test 12 - Player na posição inicial correta" (assertEqual "Test 12" Empty (player (3,3))),
            testCase "Test 13 - Player na posição inicial correta" (assertEqual "Test 13" Empty (player (4,0))),
            testCase "Test 14 - Player na posição inicial correta" (assertEqual "Test 14" Empty (player (0,1))),
            testCase "Test 15 - Player na posição inicial correta" (assertEqual "Test 15" Empty (player (7,6)))
            ]

inversePlayerTest = testGroup "inversePlayerTest" 
            [testCase "Test 1 - White to Black" (assertEqual "Test 1" Black (inversePlayer White)),
            testCase "Test 2 - Black to White" (assertEqual "Test 2" White (inversePlayer Black))
            ]

isCoordCorrectTest = testGroup "isCoordCorrectTest" 
            [testCase "Test 1 - (0,0)" (assertEqual "Test 1" True (isCoordCorrect (0,0))),
            testCase "Test 2 - (7,7)" (assertEqual "Test 2" True (isCoordCorrect (7,7))),
            testCase "Test 3 - (8,8)" (assertEqual "Test 3" False (isCoordCorrect (8,8))),
            testCase "Test 4 - (-1,-1)" (assertEqual "Test 4" False (isCoordCorrect (-1,-1)))
            ]

initialGameMock = Game {gameBoard = tabuleiro, gamePlayer = White, gameState = Running, lastClick = (-1,-1), changeRock = True}

tabuleiro = array ((0,0),(7,7)) [((0,0),Full White),((0,1),Empty),((0,2),Full White),((0,3),Empty),((0,4),Full White),((0,5),Empty),((0,6),Full White),((0,7),Empty),((1,0),Empty),((1,1),Full White),((1,2),Empty),((1,3),Full White),((1,4),Empty),((1,5),Full White),((1,6),Empty),((1,7),Full White),((2,0),Full White),((2,1),Empty),((2,2),Full White),((2,3),Empty),((2,4),Full White),((2,5),Empty),((2,6),Full White),((2,7),Empty),((3,0),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty),((3,4),Empty),((3,5),Empty),((3,6),Empty),((3,7),Empty),((4,0),Empty),((4,1),Empty),((4,2),Empty),((4,3),Empty),((4,4),Empty),((4,5),Empty),((4,6),Empty),((4,7),Empty),((5,0),Empty),((5,1),Full Black),((5,2),Empty),((5,3),Full Black),((5,4),Empty),((5,5),Full Black),((5,6),Empty),((5,7),Full Black),((6,0),Full Black),((6,1),Empty),((6,2),Full Black),((6,3),Empty),((6,4),Full Black),((6,5),Empty),((6,6),Full Black),((6,7),Empty),((7,0),Empty),((7,1),Full Black),((7,2),Empty),((7,3),Full Black),((7,4),Empty),((7,5),Full Black),((7,6),Empty),((7,7),Full Black)]

peças = [((x, y), player (x, y)) | x<-[0..7], y<-[0..7]]

nextPlayerInitialResponse = Game {gameBoard = array ((0,0),(7,7)) [((0,0),Full White),((0,1),Empty),((0,2),Full White),((0,3),Empty),((0,4),Full White),((0,5),Empty),((0,6),Full White),((0,7),Empty),((1,0),Empty),((1,1),Empty),((1,2),Empty),((1,3),Full White),((1,4),Empty),((1,5),Full White),((1,6),Empty),((1,7),Full White),((2,0),Full White),((2,1),Empty),((2,2),Full White),((2,3),Empty),((2,4),Full White),((2,5),Empty),((2,6),Full White),((2,7),Empty),((3,0),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty),((3,4),Empty),((3,5),Empty),((3,6),Empty),((3,7),Empty),((4,0),Empty),((4,1),Empty),((4,2),Empty),((4,3),Empty),((4,4),Empty),((4,5),Empty),((4,6),Empty),((4,7),Empty),((5,0),Empty),((5,1),Full Black),((5,2),Empty),((5,3),Full Black),((5,4),Empty),((5,5),Full Black),((5,6),Empty),((5,7),Full Black),((6,0),Full Black),((6,1),Empty),((6,2),Full Black),((6,3),Empty),((6,4),Full Black),((6,5),Empty),((6,6),Full Black),((6,7),Empty),((7,0),Empty),((7,1),Full Black),((7,2),Empty),((7,3),Full Black),((7,4),Empty),((7,5),Full Black),((7,6),Empty),((7,7),Full Black)], gamePlayer = Black, gameState = Running, lastClick = (-1,-1), changeRock = True}

tableJustWhite = Game {gameBoard = array ((0,0),(7,7)) [((0,0),Full White),((0,1),Empty),((0,2),Empty),((0,3),Empty),((0,4),Empty),((0,5),Empty),((0,6),Empty),((0,7),Empty),((1,0),Empty),((1,1),Empty),((1,2),Empty),((1,3),Empty),((1,4),Empty),((1,5),Empty),((1,6),Empty),((1,7),Empty),((2,0),Empty),((2,1),Empty),((2,2),Empty),((2,3),Empty),((2,4),Empty),((2,5),Empty),((2,6),Empty),((2,7),Empty),((3,0),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty),((3,4),Empty),((3,5),Empty),((3,6),Empty),((3,7),Empty),((4,0),Empty),((4,1),Empty),((4,2),Empty),((4,3),Empty),((4,4),Empty),((4,5),Empty),((4,6),Empty),((4,7),Empty),((5,0),Empty),((5,1),Empty),((5,2),Empty),((5,3),Empty),((5,4),Empty),((5,5),Empty),((5,6),Empty),((5,7),Empty),((6,0),Empty),((6,1),Empty),((6,2),Empty),((6,3),Empty),((6,4),Empty),((6,5),Empty),((6,6),Empty),((6,7),Empty),((7,0),Empty),((7,1),Empty),((7,2),Empty),((7,3),Empty),((7,4),Empty),((7,5),Empty),((7,6),Empty),((7,7),Empty)], gamePlayer = Black, gameState = Running, lastClick = (-1,-1), changeRock = True}

tableJustBlack = Game {gameBoard = array ((0,0),(7,7)) [((0,0), Empty),((0,1),Empty),((0,2),Empty),((0,3),Empty),((0,4),Empty),((0,5),Empty),((0,6),Empty),((0,7),Empty),((1,0),Empty),((1,1),Empty),((1,2),Empty),((1,3),Empty),((1,4),Empty),((1,5),Empty),((1,6),Empty),((1,7),Empty),((2,0),Empty),((2,1),Empty),((2,2),Empty),((2,3),Empty),((2,4),Empty),((2,5),Empty),((2,6),Empty),((2,7),Empty),((3,0),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty),((3,4),Empty),((3,5),Empty),((3,6),Empty),((3,7),Empty),((4,0),Empty),((4,1),Empty),((4,2),Empty),((4,3),Empty),((4,4),Empty),((4,5),Empty),((4,6),Empty),((4,7),Empty),((5,0),Empty),((5,1),Empty),((5,2),Empty),((5,3),Empty),((5,4),Empty),((5,5),Empty),((5,6),Empty),((5,7),Empty),((6,0),Empty),((6,1),Empty),((6,2),Empty),((6,3),Empty),((6,4),Empty),((6,5),Empty),((6,6),Empty),((6,7),Empty),((7,0),Empty),((7,1),Empty),((7,2),Empty),((7,3),Empty),((7,4),Empty),((7,5),Empty),((7,6),Empty),((7,7),Full Black)], gamePlayer = Black, gameState = Running, lastClick = (-1,-1), changeRock = True}

tableOneWhiteOneBlack = Game {gameBoard = array ((0,0),(7,7)) [((0,0),Full White),((0,1),Empty),((0,2),Empty),((0,3),Empty),((0,4),Empty),((0,5),Empty),((0,6),Empty),((0,7),Empty),((1,0),Empty),((1,1),Empty),((1,2),Empty),((1,3),Empty),((1,4),Empty),((1,5),Empty),((1,6),Empty),((1,7),Empty),((2,0),Empty),((2,1),Empty),((2,2),Empty),((2,3),Empty),((2,4),Empty),((2,5),Empty),((2,6),Empty),((2,7),Empty),((3,0),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty),((3,4),Empty),((3,5),Empty),((3,6),Empty),((3,7),Empty),((4,0),Empty),((4,1),Empty),((4,2),Empty),((4,3),Empty),((4,4),Empty),((4,5),Empty),((4,6),Empty),((4,7),Empty),((5,0),Empty),((5,1),Empty),((5,2),Empty),((5,3),Empty),((5,4),Empty),((5,5),Empty),((5,6),Empty),((5,7),Empty),((6,0),Empty),((6,1),Empty),((6,2),Empty),((6,3),Empty),((6,4),Empty),((6,5),Empty),((6,6),Empty),((6,7),Empty),((7,0),Empty),((7,1),Empty),((7,2),Empty),((7,3),Empty),((7,4),Empty),((7,5),Empty),((7,6),Empty),((7,7),Full Black)], gamePlayer = Black, gameState = Running, lastClick = (-1,-1), changeRock = True}

afterPossibleMoves22 = Game {gameBoard = array ((0,0),(7,7)) [((0,0),Full White),((0,1),Empty),((0,2),Full White),((0,3),Empty),((0,4),Full White),((0,5),Empty),((0,6),Full White),((0,7),Empty),((1,0),Empty),((1,1),Full White),((1,2),Empty),((1,3),Full White),((1,4),Empty),((1,5),Full White),((1,6),Empty),((1,7),Full White),((2,0),Full White),((2,1),Empty),((2,2),Full White),((2,3),Empty),((2,4),Full White),((2,5),Empty),((2,6),Full White),((2,7),Empty),((3,0),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty),((3,4),Empty),((3,5),Empty),((3,6),Empty),((3,7),Empty),((4,0),Empty),((4,1),Empty),((4,2),Empty),((4,3),Empty),((4,4),Empty),((4,5),Empty),((4,6),Empty),((4,7),Empty),((5,0),Empty),((5,1),Full Black),((5,2),Empty),((5,3),Full Black),((5,4),Empty),((5,5),Full Black),((5,6),Empty),((5,7),Full Black),((6,0),Full Black),((6,1),Empty),((6,2),Full Black),((6,3),Empty),((6,4),Full Black),((6,5),Empty),((6,6),Full Black),((6,7),Empty),((7,0),Empty),((7,1),Full Black),((7,2),Empty),((7,3),Full Black),((7,4),Empty),((7,5),Full Black),((7,6),Empty),((7,7),Full Black)], gamePlayer = White, gameState = Move, lastClick = (2,2), changeRock = True}

playerTurn22 = Game {gameBoard = array ((0,0),(7,7)) [((0,0),Full White),((0,1),Empty),((0,2),Full White),((0,3),Empty),((0,4),Full White),((0,5),Empty),((0,6),Full White),((0,7),Empty),((1,0),Empty),((1,1),Full White),((1,2),Empty),((1,3),Full White),((1,4),Empty),((1,5),Full White),((1,6),Empty),((1,7),Full White),((2,0),Full White),((2,1),Empty),((2,2),Full White),((2,3),Empty),((2,4),Full White),((2,5),Empty),((2,6),Full White),((2,7),Empty),((3,0),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty),((3,4),Empty),((3,5),Empty),((3,6),Empty),((3,7),Empty),((4,0),Empty),((4,1),Empty),((4,2),Empty),((4,3),Empty),((4,4),Empty),((4,5),Empty),((4,6),Empty),((4,7),Empty),((5,0),Empty),((5,1),Full Black),((5,2),Empty),((5,3),Full Black),((5,4),Empty),((5,5),Full Black),((5,6),Empty),((5,7),Full Black),((6,0),Full Black),((6,1),Empty),((6,2),Full Black),((6,3),Empty),((6,4),Full Black),((6,5),Empty),((6,6),Full Black),((6,7),Empty),((7,0),Empty),((7,1),Full Black),((7,2),Empty),((7,3),Full Black),((7,4),Empty),((7,5),Full Black),((7,6),Empty),((7,7),Full Black)], gamePlayer = White, gameState = Move, lastClick = (2,2), changeRock = True}
playerTurnInvalid = Game {gameBoard = array ((0,0),(7,7)) [((0,0),Full White),((0,1),Empty),((0,2),Full White),((0,3),Empty),((0,4),Full White),((0,5),Empty),((0,6),Full White),((0,7),Empty),((1,0),Empty),((1,1),Full White),((1,2),Empty),((1,3),Full White),((1,4),Empty),((1,5),Full White),((1,6),Empty),((1,7),Full White),((2,0),Full White),((2,1),Empty),((2,2),Full White),((2,3),Empty),((2,4),Full White),((2,5),Empty),((2,6),Full White),((2,7),Empty),((3,0),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty),((3,4),Empty),((3,5),Empty),((3,6),Empty),((3,7),Empty),((4,0),Empty),((4,1),Empty),((4,2),Empty),((4,3),Empty),((4,4),Empty),((4,5),Empty),((4,6),Empty),((4,7),Empty),((5,0),Empty),((5,1),Full Black),((5,2),Empty),((5,3),Full Black),((5,4),Empty),((5,5),Full Black),((5,6),Empty),((5,7),Full Black),((6,0),Full Black),((6,1),Empty),((6,2),Full Black),((6,3),Empty),((6,4),Full Black),((6,5),Empty),((6,6),Full Black),((6,7),Empty),((7,0),Empty),((7,1),Full Black),((7,2),Empty),((7,3),Full Black),((7,4),Empty),((7,5),Full Black),((7,6),Empty),((7,7),Full Black)], gamePlayer = White, gameState = Running, lastClick = (-1,-1), changeRock = True}