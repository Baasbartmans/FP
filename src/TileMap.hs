module TileMap where

import Physics
import GameObject

type TileMap = [[GameObject]]

getRigidBodies :: [GameObject] -> [RigidBody]
getRigidBodies (x:xs) = rigidBody x : getRigidBodies xs

setRigidBodies :: [GameObject] -> [RigidBody] -> [GameObject]
setRigidBodies tiles bodies = let tuples = zip tiles bodies
                              in [tile{rigidBody=body} | (tile, body) <- tuples]

update :: TileMap -> Float -> TileMap 
update oldtiles elapsedTime = let oldbodies = map (\line -> getRigidBodies line) oldtiles
                                  newbodies = map (\line -> updateRigidBodies line elapsedTime) oldbodies
                                  zipped    = zip oldtiles newbodies
                              in  map (\(tiles, bodies) -> setRigidBodies tiles bodies) zipped