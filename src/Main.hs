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
          let startGameState   = gamestates !! 2
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
                        backG = loadGameObject "Assets/spr_title.bmp" "backg" (CollisionBox False (640,360) (1,1))
                        backB = loadGameObject "Assets/spr_button_back.bmp" "backb" (CollisionBox False (640,( (520) )) (236,54))
                        playB = loadGameObject "Assets/spr_button_play.bmp" "levelSelectBtn" (CollisionBox False (640,600) (236,54))

loadVictoryScene :: IO [Scene]
loadVictoryScene = do p <- p'
                      return $ [Scene [p] [] [[]]]
                      where p' = loadGameObject "Assets/spr_victory.bmp" "victory" (CollisionBox False (640,360) (1,1))

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
                            backG = loadGameObject "Assets/spr_levelselect.bmp" "backG" (CollisionBox False (640,360) (10,10))
                            l1 = loadGameObject "Assets/spr_level_unsolved.bmp" "lvl1Btn" (CollisionBox False( 400 ,200) (500,500))
                            l2 = loadGameObject "Assets/spr_level_unsolved.bmp" "lvl2Btn" (CollisionBox False ( 540 ,200) (10,10))
                            l3 = loadGameObject "Assets/spr_level_unsolved.bmp" "level3" (CollisionBox False ( 680 ,200) (10,10))
                            l4 = loadGameObject "Assets/spr_level_unsolved.bmp" "level4" (CollisionBox False ( 820 ,200) (10,10))
                            l5 = loadGameObject "Assets/spr_level_unsolved.bmp" "level5" (CollisionBox False ( 400 ,340) (10,10))
                            l6 = loadGameObject "Assets/spr_level_unsolved.bmp" "level6" (CollisionBox False ( 540 ,340) (10,10))
                            l7 = loadGameObject "Assets/spr_level_unsolved.bmp" "level7" (CollisionBox False ( 680 ,340) (10,10))
                            l8 = loadGameObject "Assets/spr_level_unsolved.bmp" "level8" (CollisionBox False ( 820 ,340) (10,10))
                            l9 = loadGameObject "Assets/spr_level_unsolved.bmp" "level9" (CollisionBox False ( 400 , 480 ) (10,10))
                            l10 = loadGameObject "Assets/spr_level_unsolved.bmp" "level10" (CollisionBox False ( 540 ,480) (10,10))
                            l11 = loadGameObject "Assets/spr_level_unsolved.bmp" "level11" (CollisionBox False ( 680 ,480) (10,10))
                            l12 = loadGameObject "Assets/spr_level_unsolved.bmp" "level12" (CollisionBox False ( 820 ,480) (10,10))
                            backB = loadGameObject "Assets/spr_button_back.bmp" "backBtn"(CollisionBox False (640, 580 ) (236,54))
