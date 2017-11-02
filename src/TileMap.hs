module TileMap where

import Base
import Physics

data Tile = Tile {
    rigidBody :: RigidBody
}

getRigidBody :: Tile -> RigidBody
getRigidBody tile = rigidBody tile

setRigidBody :: Tile -> RigidBody -> Tile
setRigidBody tile body = Tile body

data TileMap = TileMap {
    tiles :: [Tile],
    dimensions :: Size
}

setTiles :: TileMap -> [Tile] -> TileMap
setTiles map tiles = TileMap tiles (dimensions map)

getRigidBodies :: [Tile] -> [RigidBody]
getRigidBodies (x:xs) = rigidBody x : getRigidBodies xs

setRigidBodies :: [Tile] -> [RigidBody] -> [Tile]
setRigidBodies tiles bodies = let tuples = zip tiles bodies
                              in [setRigidBody tile body | (tile, body) <- tuples]

update :: TileMap -> Float -> TileMap 
update map elapsedTime = let oldtiles  = tiles map
                             oldbodies = getRigidBodies oldtiles
                             newbodies = updateRigidBodies oldbodies elapsedTime
                             newtiles  = setRigidBodies oldtiles newbodies 
                         in  setTiles map newtiles