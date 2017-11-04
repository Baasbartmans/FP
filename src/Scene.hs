module Scene where

import TileMap

data Scene = Scene { 
    tileMap :: TileMap
}

instance Drawable Scene where
    draw scene = draw tiledmap scene