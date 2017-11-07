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
          let initialState = head (tail gamestates)
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
                        backB = loadGameObject "Assets/spr_button_back.bmp" (CollisionBox (0,( (-230) )) (100,10)) 0
                        playB = loadGameObject "Assets/spr_button_play.bmp" (CollisionBox (0,( (-160) )) (10,10)) 0


loadSelectLevel :: IO GameState
loadSelectLevel = do scene <- loadSelectLevelScene
                     return (GameState SelectLevel scene [] 0)

loadSelectLevelScene :: IO Scene
loadSelectLevelScene = do t1 <- backG
                          t2 <- l1
                          t3 <- l2
                          t4 <- l3
                          t5 <- l4
                          t6 <- l5
                          t7 <- l6
                          t8 <- l7
                          t9 <- l8
                          t10 <- l9
                          t11 <- l10
                          t12 <- l11
                          t13 <- l12
                          t14 <- backB
                          return $ Scene [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14] [] [[]]
                          where
                            backG = loadGameObject "Assets/spr_levelselect.bmp" (CollisionBox (0,0) (10,10)) 0
                            l1 = loadGameObject "Assets/spr_level_unsolved.bmp" (CollisionBox ( (-264) ,170) (10,10)) 0
                            l2 = loadGameObject "Assets/spr_level_unsolved.bmp" (CollisionBox ( (-100) ,170) (10,10)) 0
                            l3 = loadGameObject "Assets/spr_level_unsolved.bmp" (CollisionBox ( (60) ,170) (10,10)) 0
                            l4 = loadGameObject "Assets/spr_level_unsolved.bmp" (CollisionBox ( 230 ,170) (10,10)) 0
                            l5 = loadGameObject "Assets/spr_level_unsolved.bmp" (CollisionBox ( (-264) ,20) (10,10)) 0
                            l6 = loadGameObject "Assets/spr_level_unsolved.bmp" (CollisionBox ( (-100) ,20) (10,10)) 0
                            l7 = loadGameObject "Assets/spr_level_unsolved.bmp" (CollisionBox ( (60) ,20) (10,10)) 0
                            l8 = loadGameObject "Assets/spr_level_unsolved.bmp" (CollisionBox ( 230 ,20) (10,10)) 0
                            l9 = loadGameObject "Assets/spr_level_unsolved.bmp" (CollisionBox ( (-264) , (-130) ) (10,10)) 0
                            l10 = loadGameObject "Assets/spr_level_unsolved.bmp" (CollisionBox ( (-100) ,(-130)) (10,10)) 0
                            l11 = loadGameObject "Assets/spr_level_unsolved.bmp" (CollisionBox ( (60) ,(-130)) (10,10)) 0
                            l12 = loadGameObject "Assets/spr_level_unsolved.bmp" (CollisionBox ( 230 ,(-130)) (10,10)) 0
                            backB = loadGameObject "Assets/spr_button_back.bmp" (CollisionBox (0, (-280) ) (10,10)) 0

