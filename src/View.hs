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
import Data.List
import Data.Maybe

class Drawable a where
    draw :: a -> Picture

instance Drawable GameState where
    draw gstate = draw $ currentScene gstate

instance Drawable Scene where
    draw scene = let hudPicture      = drawPictures $ hud scene
                     maybePlayer     = find (\x -> "Player" == name x) (entities scene)
                     playerOffset    = if   isJust maybePlayer
                                       then getPosition (fromJust maybePlayer)
                                       else (0,0)
                     entitiesPicture = drawPicturesWithOffset (entities scene) playerOffset
                     tilemapPicture  = [drawPicturesWithOffset row playerOffset | row <- tileMap scene]
                 in  Pictures (hudPicture : entitiesPicture : tilemapPicture)

view :: GameStateManager -> IO Picture
view manager = return $ draw (current manager)

drawPictures :: [GameObject] -> Picture
drawPictures objects = let bodies         = map (\x -> body x) objects
                           noEmpties      = filter (\x -> not (isEmptyBody x)) bodies
                           tuples         = map (\x -> (collisionBox $ rigidBody x, sprite x)) noEmpties
                           inScreenCoords = toScreen tuples
                       in  Pictures [translate (fst pos) (snd pos) sprite | (pos, sprite) <- inScreenCoords]

drawPicturesWithOffset :: [GameObject] -> Position -> Picture
drawPicturesWithOffset objects playerpos = let bodies         = map (\x -> body x) objects
                                               noEmpties      = filter (\x -> not (isEmptyBody x)) bodies
                                               tuples         = map (\x -> (collisionBox $ rigidBody x, sprite x)) noEmpties
                                               inScreenCoords = toScreenWithOffset tuples playerpos
                                           in  Pictures [translate (fst pos) (snd pos) sprite | (pos, sprite) <- inScreenCoords]

toScreen :: [(CollisionBox, Picture)] -> [(Position, Picture)]
toScreen zipped = map (\(CollisionBox b (x,y) (w,h), s) -> ((x - fromIntegral(halfWidth), (-y) + fromIntegral(halfHeight)), s)) zipped

toScreenWithOffset :: [(CollisionBox, Picture)] -> Position -> [(Position, Picture)]
toScreenWithOffset zipped (px, py) = map (\(CollisionBox b (x,y) (w,h), s) -> ((x - px , (-y) + py), s)) zipped