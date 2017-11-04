module GameObject where

import Physics
import Graphics.Gloss.Data.Picture

data GameObject = GameObject {
    rigidBody :: RigidBody,
    sprite    :: Picture
}