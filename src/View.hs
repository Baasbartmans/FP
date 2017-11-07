-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Graphics.Gloss.Data.Picture
import GameObject
import Physics
import Scene

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = drawPictures (hud (scene gstate))

drawPictures :: [GameObject] -> Picture
drawPictures objects = Pictures (map (translator) objects)

translator :: GameObject -> Picture
translator GameObject{rigidBody = r, sprite = s} = translate (x) (y) $ s


--extract
getBox :: Rigidbody -> CollisionBox
getBox RigidBody{collisionBox = c} = c

getPos :: CollisionBox -> Position
getPos CollisionBox{position = p} = p

rigidX :: Position -> Float
rigidX RigidBody{x = a} = a

rigidY :: Position -> Float
rigidY RigidBody{y = a} = a

            
{-
drawPictures objects =   
    let positions  = [position (collisionBox (rigidBody obj)) | obj <- objects]
                           sprites    = [sprite obj | obj <- objects]
                           zipped     = zip positions sprites
                           translated = [Translate x y picture | ((x,y), picture) <- zipped]
                       in  Pictures translated
                       -}