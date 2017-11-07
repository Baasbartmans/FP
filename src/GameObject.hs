module GameObject where

import Physics
import Graphics.Gloss.Data.Picture

data GameObject = GameObject {
    name      :: String,
    rigidBody :: RigidBody,
    sprite    :: Picture
}

find :: String -> [GameObject] -> GameObject
find n objects = head [obj | obj <- objects, name obj == n]