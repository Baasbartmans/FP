module Physics where

import Base
import Graphics.Gloss.Data.Picture

type Bounds = (Float, Float)
type Velocity = (Float, Float)

data CollisionBox = CollisionBox {
    position :: Position,
    size     :: Size
}

instance Eq CollisionBox where
    box1 == box2 = position box1 == position box2 && size box1 == size box2
    box1 /= box2 = position box1 /= position box2 || size box1 /= size box2

data RigidBody = RigidBody {
    collisionBox  :: CollisionBox,
    addedMovement :: Velocity
}

instance Show RigidBody where
    show rigidbody = (show (collisionBox rigidbody)) ++ " | " ++ (show (addedMovement rigidbody))

instance Show CollisionBox where
    show CollisionBox{position=(x,y),size=(a,b)} = "position: " ++ show(x) ++ "," ++ show(y) ++ " size: " ++ show(a) ++ "," ++ show(b)

data GameObject = GameObject {
    name :: String,
    body :: Body
}

instance Eq GameObject where
    obj1 == obj2 = name obj1 == name obj2
    obj1 /= obj2 = name obj1 /= name obj2

data Body = Empty | Body {
    rigidBody :: RigidBody,
    sprite    :: Picture
}

isEmptyBody :: Body -> Bool
isEmptyBody Empty = True
isEmptyBody _     = False

-- COLLISION
getMinX :: CollisionBox -> Float
getMinX box = fst $ position box

getMinY :: CollisionBox -> Float
getMinY box = snd $ position box

getRigidBody :: GameObject -> RigidBody
getRigidBody object = rigidBody $ body object

setRigidBody :: GameObject -> RigidBody -> GameObject
setRigidBody obj rb = let tempbody = body obj 
                      in  obj{body=tempbody{rigidBody=rb}}

getCollisionBox :: GameObject -> CollisionBox
getCollisionBox object = collisionBox $ getRigidBody object

getPosition :: GameObject -> Position
getPosition obj = position $ getCollisionBox obj

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
                in  (not (collide xBoundb1 xBoundb2)) || (not (collide yBoundb1 yBoundb2))
                        
collide :: Bounds -> Bounds -> Bool
collide (min1, max1) (min2, max2) = if max1 < min2 then False else
                                    if min1 > max2 then False else
                                                        True

-- PHYSICS                    
updateRigidBodies :: [RigidBody] -> [RigidBody] -> Float -> [RigidBody]
updateRigidBodies bodies tmbodies elapsedTime = let updatedBodies = map (\x -> applyAddedMovement x) bodies            -- bodies to which addedMovement is added 
                                                    oldAndnew     = zip bodies updatedBodies                           -- tuples containing an old and updated body
                                                    boxes         = map (\x -> collisionBox x) (updatedBodies ++ tmbodies)
                                                    filtered      = [handleCollision combi boxes | combi <- oldAndnew] -- check if there's collision, just take the old body else take the updated one
                                                in  map (\x -> resetAddedMovement x) filtered                          -- reset the addedvelocity of each rigidbody
                                       
applyAddedMovement :: RigidBody -> RigidBody
applyAddedMovement rigidbody = let box     = collisionBox rigidbody
                                   (x,y)   = position box
                                   (vx,vy) = addedMovement rigidbody
                               in  rigidbody{collisionBox=box{position=(x + vx, y + vy)}}

testBody :: RigidBody
testBody = RigidBody (CollisionBox (0,0) (32,32)) (10,10)

testBody2 :: RigidBody
testBody2 = RigidBody (CollisionBox (10,10) (32,32)) (10,10)

handleCollision :: (RigidBody, RigidBody) -> [CollisionBox] -> RigidBody
handleCollision (old, new@RigidBody{collisionBox=box}) boxes | all (\x -> canMove box x || box == x) boxes = new -- rigidbody could move, so we keep it this way
                                                             | otherwise                                   = old -- rigidbody could not move, so we take the old body

resetAddedMovement :: RigidBody -> RigidBody
resetAddedMovement body = RigidBody (collisionBox body) (0, 0)