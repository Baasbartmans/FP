module Scene where

import TileMap
import Screen
import Physics

data Scene = Scene {
    hud      :: [GameObject],
    entities :: [GameObject],
    tileMap  :: TileMap
}