module Rendering where

import Graphics.Gloss
import Data.Array

import Data.Maybe
import Game

boardGridColor = makeColorI 0 0 0 255
playerBlackColor = makeColorI 0 0 0 255
playerMovingColor = green
playerWhiteColor = makeColorI 255 255 255 255
winnerCollor = green
validCollor = makeColorI 222 184 135 255
invalidCollor = makeColorI 245 222 179 255
tieColor = greyN 0.5

boardAsRunningPicture board =
   pictures [ color invalidCollor $ invalidCell board
            , color validCollor $ validCell board
            , color playerBlackColor $ blackCellsOfBoard board
            , color playerWhiteColor $ whiteCellsOfBoard board
            , color boardGridColor $ boardGrid
            ]

boardAsMovingPicture game
   | gamePlayer game == White = pictures [ color invalidCollor $ invalidCell board
            , color validCollor $ validCell board
            , color playerBlackColor $ blackCellsOfBoard board
            , color playerWhiteColor $ whiteCellsOfBoard board
            , color playerMovingColor $ selectedWhiteCell board cell
            , color boardGridColor boardGrid
            ]
   | gamePlayer game == Black = pictures [ color invalidCollor $ invalidCell board
            , color validCollor $ validCell board
            , color playerBlackColor $ blackCellsOfBoard board
            , color playerWhiteColor $ whiteCellsOfBoard board
            , color playerMovingColor $ selectedBlackCell board cell
            , color boardGridColor boardGrid
            ]
   | otherwise = pictures [ color invalidCollor $ invalidCell board
            , color validCollor $ validCell board
            , color playerBlackColor $ blackCellsOfBoard board
            , color playerWhiteColor $ whiteCellsOfBoard board
            , color boardGridColor boardGrid
            ]
   where board = gameBoard game
         cell = lastClick game
   --[ color playerMovingColor $ cell]

outcomeColor (Just White) = playerWhiteColor
outcomeColor (Just Black) = playerBlackColor
outcomeColor Nothing = tieColor

snapPictureToCell picture (row, column) = translate x y picture
   where x = fromIntegral column * cellWidth + cellWidth * 0.5
         y = fromIntegral row * cellHeight + cellHeight * 0.5

cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture =
   --combine all of the pictures into a single picture layer
   pictures
   --snap the picture of the cell to the corresponding cell coordinate and removing the cell value
   $ map (snapPictureToCell cellPicture .fst)
   --filtra as células de interesse
   $ filter (\(_, e) -> e == cell)
   --convert the board array to a Association list of indices and cells
   $ assocs board

selectedCell :: Board -> (Int,Int) -> Picture -> Picture
selectedCell board (x,y) cell =
   --combine all of the pictures into a single picture layer
   pictures
   --snap the picture of the cell to the corresponding cell coordinate and removing the cell value
   $ map (snapPictureToCell cell .fst)
   --filtra as células de interesse
   $ filter (\((a,b), _) -> (a,b) == (x,y))
   --convert the board array to a Association list of indices and cells
   $ assocs board
invalid (x,y) = not ((mod y 2 == 1 && mod x 2 == 1) || (mod y 2 == 0 && mod x 2 == 0))
valid (x,y) = (mod y 2 == 1 && mod x 2 == 1) || (mod y 2 == 0 && mod x 2 == 0)

invalidCell :: Board -> Picture
invalidCell board = --combine all of the pictures into a single picture layer
   pictures
   --snap the picture of the cell to the corresponding cell coordinate and removing the cell value
   $ map (snapPictureToCell (rectangleSolid cellWidth cellHeight) .fst)
   --filtra as células de interesse
   $ filter (\((a,b), _) -> invalid (a,b))
   --convert the board array to a Association list of indices and cells
   $ assocs board

validCell :: Board -> Picture
validCell board = --combine all of the pictures into a single picture layer
   pictures
   --snap the picture of the cell to the corresponding cell coordinate and removing the cell value
   $ map (snapPictureToCell (rectangleSolid cellWidth cellHeight) .fst)
   --filtra as células de interesse
   $ filter (\((a,b), _) -> valid (a,b))
   --convert the board array to a Association list of indices and cells
   $ assocs board

selectedBlackCell :: Board -> (Int, Int) -> Picture
selectedBlackCell board point = selectedCell board point cell

selectedWhiteCell :: Board -> (Int, Int) -> Picture
selectedWhiteCell board point = selectedCell board point cell

cell :: Picture
cell = thickCircle radius 15.0
   where radius = min cellWidth cellHeight * 0.25

whiteCellsOfBoard :: Board -> Picture
whiteCellsOfBoard board = cellsOfBoard board (Full White) cell

blackCellsOfBoard :: Board -> Picture
blackCellsOfBoard board = cellsOfBoard board (Full Black) cell


boardGrid :: Picture
boardGrid =
   pictures
   $ concatMap (\i -> [ line [ (i * cellWidth, 0.0),
                               (i * cellWidth, fromIntegral screenHeight)
                             ]
                      , line [ (0.0, i * cellHeight)
                             , (fromIntegral screenWidth, i * cellHeight)
                             ]
                        ])
   [0.0 .. fromIntegral n]


boardAsPicture board =
   pictures [ whiteCellsOfBoard board
   , blackCellsOfBoard board
   , boardGrid
   ]

boardAsGameOverPicture board winner
   | isNothing winner = pictures [ color tieColor $ blackCellsOfBoard board
            , color tieColor $ whiteCellsOfBoard board
            , color tieColor boardGrid
            ]
   | otherwise = pictures [ color winnerCollor $ blackCellsOfBoard board
            , color winnerCollor $ whiteCellsOfBoard board
            , color winnerCollor boardGrid
            ]
   --color (outcomeColor winner) (boardAsPicture board)

gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5)) (fromIntegral screenHeight * (-0.5)) frame
   where frame = case gameState game of
                    Move -> boardAsMovingPicture game
                    GameOver winner -> boardAsGameOverPicture (gameBoard game) winner
                    _ -> boardAsRunningPicture (gameBoard game)
