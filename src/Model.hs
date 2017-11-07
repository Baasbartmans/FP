-- | This module contains the data types
--   which represent the state of the game
module Model where

import Scene

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GSType = MainMenu | SelectLevel | Play

data GameState = GameState {
                   gsType      :: GSType,
                   scene       :: Scene,
                   gameStates  :: [GameState],
                   elapsedTime :: Float
                 }