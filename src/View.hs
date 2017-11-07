-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Graphics.Gloss.Data.Picture
import GameObject
import Physics
import Scene
import Base

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = drawPictures (hud (scene gstate))

drawPictures :: [GameObject] -> Picture
drawPictures objects = Pictures (map (translator) objects)

translator :: GameObject -> Picture
translator GameObject{rigidBody = r, sprite = s} = translate (fst pos) (snd pos) $ s
    where pos = getPos $ getBox r

--extract
getBox :: RigidBody -> CollisionBox
getBox RigidBody{collisionBox = c} = c

getPos :: CollisionBox -> Position
getPos CollisionBox{position = p} = p