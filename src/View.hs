-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Graphics.Gloss.Data.Picture
import Physics
import Scene
import Base
import Screen

class Drawable a where
    draw :: a -> Picture

instance Drawable GameState where
    draw gstate = draw $ currentScene gstate

instance Drawable Scene where
    draw scene = let hudPicture      = drawPictures $ hud scene
                     entitiesPicture = drawPictures $ entities scene
                     tilemapPicture  = [drawPictures row | row <- tileMap scene]
                 in  Pictures (hudPicture : entitiesPicture : tilemapPicture)

view :: GameStateManager -> IO Picture
view manager = return $ draw (current manager)

drawPictures :: [GameObject] -> Picture
drawPictures objects = let bodies         = map (\x -> body x) objects
                           noEmpties      = filter (\x -> (not . isEmptyBody) x) bodies
                           tuples         = map (\x -> (collisionBox $ rigidBody x, sprite x)) noEmpties
                           inScreenCoords = toScreen tuples
                       in  Pictures [translate (fst pos) (snd pos) sprite | (pos, sprite) <- inScreenCoords]

toScreen :: [(CollisionBox, Picture)] -> [(Position, Picture)]
toScreen zipped = map (\(CollisionBox (x,y) (w,h), s) -> ((x - fromIntegral(halfWidth), (-y) + fromIntegral(halfHeight)), s)) zipped