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
import Screen

main :: IO ()
main = do gamestates <- loadGameStates
          let startGameState   = gamestates !! 1
          let gameStateManager = GameStateManager startGameState gamestates 0
          playIO window        -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              gameStateManager -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
              
window :: Display
window = InWindow "Pong" (width, height) (0, 0)
        
loadGameStates :: IO [GameState]
loadGameStates = do mainMenu    <- loadGameState MainMenu loadMainMenuScene
                    selectLevel <- loadGameState SelectLevel loadSelectLevelScene 
                    playState   <- loadGameState Play (loadLevels ["Levels/" ++ show int ++ ".txt" | int <- [2..2]])
                    victory     <- loadGameState Victory loadVictoryScene
                    return [mainMenu, selectLevel, playState, victory]

loadGameState :: GSType -> IO [Scene] -> IO GameState
loadGameState gstype scenes = (\x -> GameState (head x) x gstype) <$> scenes

loadMainMenuScene :: IO [Scene]
loadMainMenuScene = do t1 <- backG
                       t2 <- backB
                       t3 <- playB
                       return $ [Scene [t1, t2, t3] [] [[]]]
                       where
<<<<<<< HEAD
                        backG = loadGameObject "Assets/spr_title.bmp" "backg" (CollisionBox False (0,0) (10,10))
                        backB = loadGameObject "Assets/spr_button_back.bmp" "backb" (CollisionBox False (0,( (-230) )) (236,54))
                        playB = loadGameObject "Assets/spr_button_play.bmp" "levelSelectBtn" (CollisionBox False (0,-160) (236,54))
=======
                        backG = loadGameObject "Assets/spr_title.bmp" "backg" (CollisionBox (640,360) (1,1))
                        backB = loadGameObject "Assets/spr_button_back.bmp" "backb" (CollisionBox (640,( (520) )) (236,54))
                        playB = loadGameObject "Assets/spr_button_play.bmp" "levelSelectBtn" (CollisionBox (640,600) (236,54))

loadVictoryScene :: IO [Scene]
loadVictoryScene = do p <- p'
                      return $ [Scene [p] [] [[]]]
                      where p' = loadGameObject "Assets/spr_victory.bmp" "victory" (CollisionBox (640,360) (1,1))
>>>>>>> 63fc6520ebb74e5b62e9a5ae4790821a9d9ea864

loadSelectLevelScene :: IO [Scene]
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
                          return $ [Scene [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14] [] [[]]]
                          where
<<<<<<< HEAD
                            backG = loadGameObject "Assets/spr_levelselect.bmp" "backG" (CollisionBox False (0,0) (10,10))
                            l1 = loadGameObject "Assets/spr_level_unsolved.bmp" "lvl1Btn" (CollisionBox False ( (-264) ,170) (200,200))
                            l2 = loadGameObject "Assets/spr_level_unsolved.bmp" "lvl2Btn" (CollisionBox False ( (-100) ,170) (10,10))
                            l3 = loadGameObject "Assets/spr_level_unsolved.bmp" "level3" (CollisionBox False ( (60) ,170) (10,10))
                            l4 = loadGameObject "Assets/spr_level_unsolved.bmp" "level4" (CollisionBox False ( 230 ,170) (10,10))
                            l5 = loadGameObject "Assets/spr_level_unsolved.bmp" "level5" (CollisionBox False ( (-264) ,20) (10,10))
                            l6 = loadGameObject "Assets/spr_level_unsolved.bmp" "level6" (CollisionBox False ( (-100) ,20) (10,10))
                            l7 = loadGameObject "Assets/spr_level_unsolved.bmp" "level7" (CollisionBox False ( (60) ,20) (10,10))
                            l8 = loadGameObject "Assets/spr_level_unsolved.bmp" "level8" (CollisionBox False ( 230 ,20) (10,10))
                            l9 = loadGameObject "Assets/spr_level_unsolved.bmp" "level9" (CollisionBox False ( (-264) , (-130) ) (10,10))
                            l10 = loadGameObject "Assets/spr_level_unsolved.bmp" "level10" (CollisionBox False ( (-100) ,(-130)) (10,10))
                            l11 = loadGameObject "Assets/spr_level_unsolved.bmp" "level11" (CollisionBox False ( (60) ,(-130)) (10,10))
                            l12 = loadGameObject "Assets/spr_level_unsolved.bmp" "level12" (CollisionBox False ( 230 ,(-130)) (10,10))
                            backB = loadGameObject "Assets/spr_button_back.bmp" "backBtn"(CollisionBox False (0, (-280) ) (236,54))
=======
                            backG = loadGameObject "Assets/spr_levelselect.bmp" "backG" (CollisionBox (640,360) (10,10))
                            l1 = loadGameObject "Assets/spr_level_unsolved.bmp" "lvl1Btn" (CollisionBox ( 400 ,200) (200,200))
                            l2 = loadGameObject "Assets/spr_level_unsolved.bmp" "lvl2Btn" (CollisionBox ( 540 ,200) (10,10))
                            l3 = loadGameObject "Assets/spr_level_unsolved.bmp" "level3" (CollisionBox ( 680 ,200) (10,10))
                            l4 = loadGameObject "Assets/spr_level_unsolved.bmp" "level4" (CollisionBox ( 820 ,200) (10,10))
                            l5 = loadGameObject "Assets/spr_level_unsolved.bmp" "level5" (CollisionBox ( 400 ,340) (10,10))
                            l6 = loadGameObject "Assets/spr_level_unsolved.bmp" "level6" (CollisionBox ( 540 ,340) (10,10))
                            l7 = loadGameObject "Assets/spr_level_unsolved.bmp" "level7" (CollisionBox ( 680 ,340) (10,10))
                            l8 = loadGameObject "Assets/spr_level_unsolved.bmp" "level8" (CollisionBox ( 820 ,340) (10,10))
                            l9 = loadGameObject "Assets/spr_level_unsolved.bmp" "level9" (CollisionBox ( 400 , 480 ) (10,10))
                            l10 = loadGameObject "Assets/spr_level_unsolved.bmp" "level10" (CollisionBox ( 540 ,480) (10,10))
                            l11 = loadGameObject "Assets/spr_level_unsolved.bmp" "level11" (CollisionBox ( 680 ,480) (10,10))
                            l12 = loadGameObject "Assets/spr_level_unsolved.bmp" "level12" (CollisionBox ( 820 ,480) (10,10))
                            backB = loadGameObject "Assets/spr_button_back.bmp" "backBtn"(CollisionBox (640, 580 ) (236,54))
>>>>>>> 63fc6520ebb74e5b62e9a5ae4790821a9d9ea864
