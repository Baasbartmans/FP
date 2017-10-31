module Physics where

import Base

type Bounds = (Float, Float)

data CollisionBox = CollisionBox {
    position :: Position,
    size :: Size
}

data RigidBody = RigidBody {
    collisionBox :: CollisionBox,
    velocity :: (Float, Float),
    weight :: Float
}

-- COLLISION
getMinX :: CollisionBox -> Float
getMinX box = fst $ position box

getMinY :: CollisionBox -> Float
getMinY box = snd $ position box

-- haakjes weghalen vragen op werkcollege!!!
getXBounds :: CollisionBox -> Bounds
getXBounds box = let minx = getMinX box
               in (minx, minx + fromIntegral (width (size box)))

getYBounds :: CollisionBox -> Bounds
getYBounds box = let miny = getMinY box
               in (miny, miny + fromIntegral (height (size box)))

canMove :: CollisionBox -> CollisionBox -> Bool
canMove b1 b2 = let xBoundb1 = getXBounds b1
                    yBoundb1 = getYBounds b1
                    xBoundb2 = getXBounds b2
                    yBoundb2 = getYBounds b2
                in  not (collide xBoundb1 xBoundb2) && not (collide yBoundb1 yBoundb2)
                        
collide :: Bounds -> Bounds -> Bool
collide (min1, max1) (min2, max2) = if max1 < min2 then False else
                                    if min1 > max2 then False else  
                                                        True

-- PHYSICS                                                              
updateRigidBodies :: [RigidBody] -> [RigidBody]
updateRigidBodies bodies = let tuples = [(body, applyPhysics body) | body <- bodies]
                               boxes  = [collisionBox body | body <- bodies] 
                           in  map (handleCollision boxes) tuples

applyPhysics :: RigidBody -> Position
applyPhysics RigidBody {collisionBox = box, velocity = v, weight = w} = applyFunctionToTuple v (\x -> w + w * x)

handleCollision :: [CollisionBox] -> (RigidBody, Position) -> RigidBody
handleCollision boxes (body, newPos) = let newbox = setPosition (collisionBox body) newPos
                                       in  if all (\x -> canMove newbox x) boxes
                                           then setCollisionBox body newbox
                                           else body

setPosition :: CollisionBox -> Position -> CollisionBox
setPosition box newPos = CollisionBox{position = newPos, size = size box}

setCollisionBox :: RigidBody -> CollisionBox -> RigidBody
setCollisionBox body box = RigidBody {collisionBox = box, velocity = velocity body, weight = weight body}

applyFunctionToTuple :: (a, a) -> (a -> b) -> (b, b)
applyFunctionToTuple (a, b) f = (f a, f b)