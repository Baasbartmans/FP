module Main where

import Controller
import Model
import View
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import Scene 
import System.Random
import ObjectFactory
import Physics

main :: IO ()
main = do gamestates <- loadGameStates
          let initialState = head gamestates
          playIO (InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function

width, height, offset :: Int
width = 300
height = 300
offset = 100
              
window :: Display
window = InWindow "Pong" (width, height) (offset, offset)
        
loadGameStates :: IO [GameState]
loadGameStates = do mainMenu    <- loadMainMenu
                    selectLevel <- loadSelectLevel
                    let finishedMainMenu = mainMenu{gameStates=[selectLevel]}
                    return (mainMenu : selectLevel : [])

loadMainMenu :: IO GameState
loadMainMenu = do scene <- loadMainMenuScene
                  return (GameState MainMenu scene [] 0)

loadMainMenuScene :: IO Scene
loadMainMenuScene = do t1 <- backG
                       t2 <- backB
                       t3 <- playB

                       return $ Scene [t1, t2, t3] [] [[]]

                       where
                        backG = loadGameObject "Assets/spr_title.bmp" (CollisionBox (0,0) (10,10)) 0
                        backB = loadGameObject "Assets/spr_button_back.bmp" (CollisionBox (25,120) (100,10)) 0
                        playB = loadGameObject "Assets/spr_button_play.bmp" (CollisionBox (25,160) (10,10)) 0


loadSelectLevel :: IO GameState
loadSelectLevel = do scene <- loadSelectLevelScene
                     return (GameState SelectLevel scene [] 0)

loadSelectLevelScene :: IO Scene
loadSelectLevelScene = do randomNumber <- randomIO :: IO Int 
                          return $ Scene [] [] [[]]