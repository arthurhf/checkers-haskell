module Logic where
import Data.Array
import Game
import Graphics.Gloss.Interface.Pure.Game
{-
TODO
  1 - Clicar na pedra abre as opções de movimentação FEATURE FUTURA
  2 - Comer pedra adversária OK
  3 - Guardar quantidade de pedras de cada jogador para verificar quem ganhou OK
  4 - Quem ganhou OK
  5 - Comer em sequencia OK
  6 - Comer para tras OK
  7 - Mostrar quem ganhou OK
  8 - Modificar visualização de quem ganhou OK
-}

isCoordCorrect = inRange ((0,0), (n-1, n-1))

inversePlayer :: Player -> Player
inversePlayer player
   | player == Black = White
   | otherwise = Black

nextPlayer :: Game -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Game
nextPlayer game (x,y) (a,b) lastCell
   | isCoordCorrect (x+1, y+1) && board ! (x+1, y+1) == (Full $ inversePlayer player) && isCoordCorrect (x+2, y+2) && board ! (x+2, y+2) == Empty = game {gameBoard = board // [(lastCell, Empty), ((a, b), Empty), ((x,y), Full $ player)],  gamePlayer = player, gameState = Move, lastClick = (x,y), changeRock = False}
   | isCoordCorrect (x-1, y+1) && board ! (x-1, y+1) == (Full $ inversePlayer player) && isCoordCorrect (x-2, y+2) && board ! (x-2, y+2) == Empty = game {gameBoard = board // [(lastCell, Empty), ((a, b), Empty), ((x,y), Full $ player)],  gamePlayer = player, gameState = Move, lastClick = (x,y), changeRock = False}
   | isCoordCorrect (x-1, y-1) && board ! (x-1, y-1) == (Full $ inversePlayer player) && isCoordCorrect (x-2, y-2) && board ! (x-2, y-2) == Empty = game {gameBoard = board // [(lastCell, Empty), ((a, b), Empty), ((x,y), Full $ player)],  gamePlayer = player, gameState = Move, lastClick = (x,y), changeRock = False}
   | isCoordCorrect (x+1, y-1) && board ! (x+1, y-1) == (Full $ inversePlayer player) && isCoordCorrect (x+2, y-2) && board ! (x+2, y-2) == Empty = game {gameBoard = board // [(lastCell, Empty), ((a, b), Empty), ((x,y), Full $ player)],  gamePlayer = player, gameState = Move, lastClick = (x,y), changeRock = False}
   | otherwise = game {gameBoard = board // [(lastCell, Empty), ((a,b), Empty), ((x, y), Full $ player)],  gamePlayer = inversePlayer player, gameState = Running, changeRock = True}
   where board = gameBoard game
         player = gamePlayer game

-- Possible Moves starts
-- a ideia dessa função é: se tem pra onde ir, seta o gameState pra Move
possibleMoves :: Game -> (Int, Int) -> Game
possibleMoves game coord
      | gamePlayer game == White = possibleMovesWhite game coord
      | otherwise = possibleMovesBlack game coord

possibleMovesWhite :: Game -> (Int, Int) -> Game
possibleMovesWhite game (x,y)
   | isCoordCorrect (x+1, y+1) && board ! (x+1, y+1) == (Full $ inversePlayer player) && isCoordCorrect (x+2,y+2) && board ! (x+2,y+2) == Empty = game {gameBoard = board, gameState = Move, lastClick = (x,y)}
   | isCoordCorrect (x+1, y-1) && board ! (x+1, y-1) == (Full $ inversePlayer player) && isCoordCorrect (x+2,y-2) && board ! (x+2,y-2) == Empty = game {gameBoard = board, gameState = Move, lastClick = (x,y)}
   | isCoordCorrect (x-1, y-1) && board ! (x-1, y-1) == (Full $ inversePlayer player) && isCoordCorrect (x-2,y-2) && board ! (x-2,y-2) == Empty = game {gameBoard = board, gameState = Move, lastClick = (x,y)}
   | isCoordCorrect (x-1, y+1) && board ! (x-1, y+1) == (Full $ inversePlayer player) && isCoordCorrect (x-2,y+2) && board ! (x-2,y+2) == Empty = game {gameBoard = board, gameState = Move, lastClick = (x,y)}
   | isCoordCorrect (x+1, y-1) && board ! (x+1, y-1) == Empty = game {gameBoard = board, gameState = Move, lastClick = (x,y)}
   | isCoordCorrect (x+1, y+1) && board ! (x+1, y+1) == Empty = game {gameBoard = board, gameState = Move, lastClick = (x,y)}
   | otherwise = game
   where board = gameBoard game
         player = gamePlayer game
         lastCell = lastClick game

possibleMovesBlack :: Game -> (Int, Int) -> Game
possibleMovesBlack game (x,y)
   | isCoordCorrect (x+1, y+1) && board ! (x+1, y+1) == (Full $ inversePlayer player) && isCoordCorrect (x+2,y+2) && board ! (x+2,y+2) == Empty = game {gameBoard = board, gameState = Move, lastClick = (x,y)}
   | isCoordCorrect (x+1, y-1) && board ! (x+1, y-1) == (Full $ inversePlayer player) && isCoordCorrect (x+2,y-2) && board ! (x+2,y-2) == Empty = game {gameBoard = board, gameState = Move, lastClick = (x,y)}
   | isCoordCorrect (x-1, y-1) && board ! (x-1, y-1) == (Full $ inversePlayer player) && isCoordCorrect (x-2,y-2) && board ! (x-2,y-2) == Empty = game {gameBoard = board, gameState = Move, lastClick = (x,y)}
   | isCoordCorrect (x-1, y+1) && board ! (x-1, y+1) == (Full $ inversePlayer player) && isCoordCorrect (x-2,y+2) && board ! (x-2,y+2) == Empty = game {gameBoard = board, gameState = Move, lastClick = (x,y)}
   | isCoordCorrect (x-1, y-1) && board ! (x-1, y-1) == Empty = game {gameBoard = board, gameState = Move, lastClick = (x,y)}
   | isCoordCorrect (x-1, y+1) && board ! (x-1, y+1) == Empty = game {gameBoard = board, gameState = Move, lastClick = (x,y)}
   | otherwise = game
   where board = gameBoard game
         player = gamePlayer game
         lastCell = lastClick game

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
   | gameStateAfterMove game == GameOver (Just White) = game {gameState = GameOver (Just White)}
   | gameStateAfterMove game == GameOver (Just Black) = game {gameState = GameOver (Just Black)}
   | gameStateAfterMove game == GameOver Nothing = game {gameState = GameOver Nothing}
   | isCoordCorrect cellCoord && board ! cellCoord == (Full $ player) = possibleMoves game cellCoord
   | otherwise = game
   where board = gameBoard game
         player = gamePlayer game

--MOVE ROCK STARTS
-- achei o bug: se no segundo click a pedra for pra borda, o pattern só manda a pedra pra posição.
-- verifica se teve alguém no caminho EM TODOS os casos de movimentação
-- movimenta a peça e seta o gameState pra Running e espera a próxima jogada
-- guardar no gameState a peça anterior
moveRock :: Game -> (Int, Int) -> Game
moveRock game cellCoord = if isCoordCorrect cellCoord then if player == White then moveRockWhite game cellCoord else moveRockBlack game cellCoord else game
   where player = gamePlayer game

moveRockBlack :: Game -> (Int, Int) -> Game
moveRockBlack game (x,y)
   | isCoordCorrect (x+1, y+1) && board ! (x+1, y+1) == (Full $ inversePlayer player) && isCoordCorrect (x+2,y+2) && (x+2,y+2) == lastCell && board ! (x, y) == Empty  = nextPlayer game (x,y) (x+1, y+1) lastCell
   | isCoordCorrect (x+1, y-1) && board ! (x+1, y-1) == (Full $ inversePlayer player) && isCoordCorrect (x+2,y-2) && (x+2,y-2) == lastCell && board ! (x, y) == Empty  = nextPlayer game (x,y) (x+1, y-1) lastCell
   | isCoordCorrect (x-1, y-1) && board ! (x-1, y-1) == (Full $ inversePlayer player) && isCoordCorrect (x-2,y-2) && (x-2,y-2) == lastCell && board ! (x, y) == Empty  = nextPlayer game (x,y) (x-1,y-1) lastCell
   | isCoordCorrect (x-1, y+1) && board ! (x-1, y+1) == (Full $ inversePlayer player) && isCoordCorrect (x-2,y+2) && (x-2,y+2) == lastCell && board ! (x, y) == Empty = nextPlayer game (x,y) (x-1,y+1) lastCell
   | ((x+1,y-1)==lastCell || (x+1, y+1) == lastCell) && board ! (x, y) == Empty  = game {gameBoard = board // [((x, y), Full $ player), (lastCell, Empty)], gamePlayer = inversePlayer $ gamePlayer game, gameState = gameStateAfterMove game}
   | not (changeRock game) = game
   | otherwise = game {gameState = gameStateAfterMove game}
   where board = gameBoard game
         player = gamePlayer game
         lastCell = lastClick game

moveRockWhite :: Game -> (Int, Int) -> Game
moveRockWhite game (x,y)
   | isCoordCorrect (x+1, y+1) && board ! (x+1, y+1) == (Full $ inversePlayer player) && isCoordCorrect (x+2,y+2) && (x+2,y+2) == lastCell && board ! (x, y) == Empty  = nextPlayer game (x,y) (x+1, y+1) lastCell
   | isCoordCorrect (x+1, y-1) && board ! (x+1, y-1) == (Full $ inversePlayer player) && isCoordCorrect (x+2,y-2) && (x+2,y-2) == lastCell && board ! (x, y) == Empty  = nextPlayer game (x,y) (x+1, y-1) lastCell
   | isCoordCorrect (x-1, y-1) && board ! (x-1, y-1) == (Full $ inversePlayer player) && isCoordCorrect (x-2,y-2) && (x-2,y-2) == lastCell && board ! (x, y) == Empty  = nextPlayer game (x,y) (x-1,y-1) lastCell
   | isCoordCorrect (x-1, y+1) && board ! (x-1, y+1) == (Full $ inversePlayer player) && isCoordCorrect (x-2,y+2) && (x-2,y+2) == lastCell && board ! (x, y) == Empty = nextPlayer game (x,y) (x-1,y+1) lastCell
   | ((x-1,y-1)==lastCell || (x-1, y+1) == lastCell) && board ! (x, y) == Empty  = game {gameBoard = board // [((x, y), Full $ player), (lastCell, Empty)], gamePlayer = inversePlayer $ gamePlayer game, gameState = gameStateAfterMove game}
   | not (changeRock game) = game
   | otherwise = game {gameState = gameStateAfterMove game}
   where board = gameBoard game
         player = gamePlayer game
         lastCell = lastClick game

gameStateAfterMove :: Game -> State
gameStateAfterMove game
   | blackPieces == 0 = GameOver (Just White)
   | whitePieces == 0 = GameOver (Just Black)
   | allPieces == 2 = GameOver Nothing
   | otherwise = Running
   where
      countPieces :: Cell -> [Cell] -> Int
      countPieces player xs = length $ filter (== player) xs
      blackPieces :: Int
      blackPieces = countPieces (Full Black) (elems $ gameBoard game)
      whitePieces :: Int
      whitePieces = countPieces (Full White) (elems $ gameBoard game)
      allPieces :: Int
      allPieces = blackPieces + whitePieces

mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x,y) = ( floor ((y + (fromIntegral screenHeight * 0.5))/cellHeight) -- converte coordenadas String em coordenadas do tabuleiro
                            , floor ((x + (fromIntegral screenWidth * 0.5))/cellWidth )
                            )

transformGame :: Event -> Game -> Game
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
   case gameState game of
      Running -> playerTurn game $ mousePosAsCellCoord mousePos
      Move -> moveRock game $ mousePosAsCellCoord mousePos
      GameOver _ -> initialGame
transformGame _ game = game
