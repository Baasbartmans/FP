module Physics where

import Base
import Graphics.Gloss.Data.Picture

type Bounds = (Float, Float)
type Velocity = (Float, Float)

data CollisionBox = CollisionBox {
    position :: Position,
    size     :: Size
}

data RigidBody = RigidBody {
    collisionBox  :: CollisionBox,
    velocity      :: Velocity,
    addedvelocity :: Velocity,    
    weight        :: Float
}

instance Show CollisionBox where
    show CollisionBox{position=(x,y),size=(a,b)} = "position: " ++ show(x) ++ "," ++ show(y) ++ " size: " ++ show(a) ++ "," ++ show(b)

data GameObject = GameObject {
    name      :: String,
    rigidBody :: RigidBody,
    sprite    :: Picture
}

find :: String -> [GameObject] -> GameObject
find n objects = head [obj | obj <- objects, name obj == n]

-- COLLISION
getMinX :: CollisionBox -> Float
getMinX box = fst $ position box

getMinY :: CollisionBox -> Float
getMinY box = snd $ position box

getCollisionBox :: GameObject -> CollisionBox
getCollisionBox object = collisionBox (rigidBody object)

-- haakjes weghalen vragen op werkcollege!!!
getXBounds :: CollisionBox -> Bounds
getXBounds box = let minx = getMinX box
                 in (minx, minx + fromIntegral (fst (size box)))

getYBounds :: CollisionBox -> Bounds
getYBounds box = let miny = getMinY box
                 in (miny, miny + fromIntegral (snd (size box)))

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
gravityConst :: Float
gravityConst = 9.81

updateRigidBodies :: [RigidBody] -> Float -> [RigidBody]
updateRigidBodies bodies elapsedTime = let updated  = [applyPhysics body elapsedTime | body <- bodies]    -- apply physics and addedMovement
                                           oldAnew  = zip bodies updated                                  -- a tuple containing an old and updated body
                                           newboxes = [collisionBox body | body <- updated]               -- the collisionboxes of the updated bodies
                                           filtered = [handleCollision combi newboxes | combi <- oldAnew] -- check if there's collision, just take the old body else take the updated one
                                       in  [resetAddedVelocity body | body <- filtered]                   -- reset the addedvelocity of each rigidbody

applyPhysics :: RigidBody -> Float -> RigidBody
applyPhysics body@RigidBody {collisionBox=box, 
                             velocity=(vx, vy), 
                             addedvelocity=(vax, vay), 
                             weight=w} elapsedTime = let gravity     = vy + elapsedTime * w * gravityConst
                                                         newvelocity = (vx + vax * elapsedTime, vy + vay * elapsedTime + gravity)
                                                         newbox      = addVelocity box newvelocity elapsedTime
                                                     in  body {collisionBox=newbox}

handleCollision :: (RigidBody, RigidBody) -> [CollisionBox] -> RigidBody
handleCollision (old, new) boxes | all (\x -> canMove (collisionBox new) x) boxes = new -- rigidbody could move, so we keep it this way
                                 | otherwise                                      = old -- rigidbody could not move, so we take the old body

resetAddedVelocity :: RigidBody -> RigidBody
resetAddedVelocity body = RigidBody (collisionBox body) (velocity body) (0, 0) (weight body)

addVelocity :: CollisionBox -> Velocity -> Float -> CollisionBox
addVelocity box@CollisionBox {position=(x, y)} (vx, vy) elapsedTime = box {position=(x + vx * elapsedTime, y + vy * elapsedTime)}