-- | This module contains the data types
--   which represent the state of the game
module Model where

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   scene       :: Scene,
                   elapsedTime :: Float
                 }

initialScene :: Scene
initialScene ::

initialState :: GameState
initialState = GameState ShowNothing 0