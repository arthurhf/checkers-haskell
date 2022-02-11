module Game where

import Data.Array

data Player = White | Black deriving (Eq, Show)
data Cell = Empty | Full Player deriving (Eq, Show)
data State = Move | Running | GameOver (Maybe Player) deriving (Eq, Show)

type Board = Array (Int, Int) Cell
data Game = Game { gameBoard :: Board
                 , gamePlayer :: Player
                 , gameState :: State
                 , lastClick :: (Int, Int)
                 , changeRock :: Bool
                 } deriving (Eq, Show)
n :: Int
n = 8

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight::Float
cellHeight = fromIntegral screenHeight / fromIntegral n

initialGame = Game { gameBoard = initialGameBoard
                   , gamePlayer = White
                   , gameState = Running
                   , lastClick = (-1,-1)
                   , changeRock = True
                  }

initialGameBoard :: Board
initialGameBoard = (array indexRange peçasIniciais)
{--[((0,0), Full White), ((1,1), Full White), ((7,7), Full Black), ((6,6), Full Black), ((7,5), Full Black), ((7,3), Full Black), ((7,1), Full Black), ((6,4), Full Black), ((6,2), Full Black), ((6,0), Full Black)] --}
   where indexRange = ((0,0), (n-1, n-1))

peçasIniciais :: [((Int, Int),Cell)]
peçasIniciais = [((x, y), player (x, y)) | x<-[0..7], y<-[0..7]]
{--   zip (range indexRange) [((0,0), Full White), ((1,1), Full White), ((7,7), Full Black), ((6,6), Full Black), ((7,5), Full Black), ((7,3), Full Black), ((7,1), Full Black), ((6,4), Full Black), ((6,2), Full Black), ((6,0), Full Black)]
 --}  where indexRange = ((0,0), (n-1, n-1))

player :: (Int, Int) -> Cell
player (x,y)
   | piece && x <= 2 = Full White
   | piece && x >= 5 = Full Black
   | otherwise = Empty
   where
      piece = (mod y 2 == 1 && mod x 2 == 1) || (mod y 2 == 0 && mod x 2 == 0)
