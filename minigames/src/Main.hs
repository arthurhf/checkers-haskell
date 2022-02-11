module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Game
import Logic
import Rendering

window = InWindow "Jogo de Damas" (640, 480) (100, 100)
backgroundColor = orange

main :: IO ()
main = play window backgroundColor 30  initialGame gameAsPicture transformGame (\_->id)
