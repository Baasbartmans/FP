module Main where

import Controller
import Model
import View
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Graphics.Gloss.Interface.IO.Game
{-
main :: IO ()
main = playIO (InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
-}
width, height, offset :: Int
width = 300
height = 300
offset = 100
              
window :: Display
window = InWindow "Pong" (width, height) (offset, offset)
              
background :: Color
background = black
              
main :: IO ()
main = do
    -- load alle plaatjes
    player <- loadBMP "D:/floor.bmp"
                  
    --teken de plaatjes
    display window background (pictures [
        translate 30 50 $ player])