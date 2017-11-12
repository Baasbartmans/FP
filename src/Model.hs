-- | This module contains the data types
--   which represent the state of the game
module Model where

import Scene
import Data.List
import Data.Maybe

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameStateManager = GameStateManager {
  current     :: GameState,
  gameStates  :: [GameState],
  elapsedTime :: Float  
}

data GSType = MainMenu | SelectLevel | Play deriving (Eq)
data GameState = GameState {
  currentScene :: Scene,
  scenes       :: [Scene],               
  gsType       :: GSType
}

getGameState :: GameStateManager -> GSType -> GameState
getGameState manager gstype = case find (\x -> gsType x == gstype) (gameStates manager) of
                                   Just x  -> x
                                   Nothing -> error "GameStateType is not known"