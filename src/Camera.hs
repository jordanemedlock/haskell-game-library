{-|
Module      : Camera
Description : The Camera object.
Copyright   : (c) Jordan Medlock 2015
Maintainer  : jordanemedlock@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Camera (
  Camera(..),
  newCamera,
  updateCam,
  lookThrough,
  cameraKeyDown,
  cameraKeyUp,
  X,Y,Z,
  Yaw,Pitch,Roll
) where

import Graphics.UI.GLUT
import Graphics.Rendering.GLU.Raw (gluLookAt)
import Data.Time.Clock

type X = GLdouble
type Y = GLdouble
type Z = GLdouble

type Yaw = GLdouble
type Pitch = GLdouble
type Roll = GLdouble


-- |The 'Camera' structure holds all of the necessary information to dictate 
-- where the camera should be placed and how.
data Camera = Camera { camPosition :: (X, Y, Z),
                       camMoving :: (X,Y,Z),
                       camRotation :: (Roll,Pitch,Yaw),
                       camTurning :: (Roll,Pitch,Yaw)
                     }

-- |The 'newCamera' function creates a camera with the default values.
newCamera :: X -> Y -> Z -> Camera
newCamera x y z = Camera (x,y,z) (0,0,0) (0,0,0) (0,0,0)

-- |The 'updateCam' function should be called every idle frame. At the moment 
-- it doesn't do anything and is equivalent to 'id'.
updateCam :: NominalDiffTime -> Camera -> Camera
updateCam dt = updateRot dt . updatePos dt

updateRot :: NominalDiffTime -> Camera -> Camera
updateRot dt c = checkPitch c'
  where (r,p,y) = camRotation c
        dt' = realToFrac dt
        (dr,dp,dy) = camTurning c
        c' = c {camRotation = (r + dr*rotSpeed*dt', p + dp*rotSpeed*dt', y + dy*rotSpeed*dt')}

updatePos :: NominalDiffTime -> Camera -> Camera
updatePos dt cam@(Camera (x,y,z) (-1,dy,-1) (_,_,yaw') _) = cam {camPosition = (x + scaleX dt (yaw'+pi/4), y + dy*speed*(realToFrac dt), z - scaleZ dt (yaw'+pi/4))} -- NorthWest
updatePos dt cam@(Camera (x,y,z) ( 0,dy,-1) (_,_,yaw') _) = cam {camPosition = (x + scaleX dt yaw',        y + dy*speed*(realToFrac dt), z - scaleZ dt yaw')}        -- North
updatePos dt cam@(Camera (x,y,z) ( 1,dy,-1) (_,_,yaw') _) = cam {camPosition = (x + scaleX dt (yaw'-pi/4), y + dy*speed*(realToFrac dt), z - scaleZ dt (yaw'-pi/4))} -- NorthEast
updatePos dt cam@(Camera (x,y,z) (-1,dy, 0) (_,_,yaw') _) = cam {camPosition = (x + scaleX dt (yaw'+pi/2), y + dy*speed*(realToFrac dt), z - scaleZ dt (yaw'+pi/2))} -- West
updatePos dt cam@(Camera (x,y,z) ( 0,dy, 0) (_,_, _  ) _) = cam {camPosition = (x                        , y + dy*speed*(realToFrac dt), z                        )} -- Not Moving
updatePos dt cam@(Camera (x,y,z) ( 1,dy, 0) (_,_,yaw') _) = cam {camPosition = (x + scaleX dt (yaw'-pi/2), y + dy*speed*(realToFrac dt), z - scaleZ dt (yaw'-pi/2))} -- East
updatePos dt cam@(Camera (x,y,z) (-1,dy, 1) (_,_,yaw') _) = cam {camPosition = (x - scaleX dt (yaw'-pi/4), y + dy*speed*(realToFrac dt), z + scaleZ dt (yaw'-pi/4))} -- SouthWest
updatePos dt cam@(Camera (x,y,z) ( 0,dy, 1) (_,_,yaw') _) = cam {camPosition = (x - scaleX dt yaw',        y + dy*speed*(realToFrac dt), z + scaleZ dt yaw')}        -- South
updatePos dt cam@(Camera (x,y,z) ( 1,dy, 1) (_,_,yaw') _) = cam {camPosition = (x - scaleX dt (yaw'+pi/4), y + dy*speed*(realToFrac dt), z + scaleZ dt (yaw'+pi/4))} -- SouthEast




-- |The 'lookThrough' function uses the cameras position to move the 3D environment
-- to the cameras perspective.
lookThrough :: Camera -> IO ()
lookThrough c = do
  let (x,y,z) = camPosition c
  let (_,pitch',yaw') = camRotation c
  let dx = realToFrac $ sin(yaw')
  let dy = realToFrac $ -sin(pitch')
  let dz = realToFrac $ -cos(yaw')

  gluLookAt x y z
            (x+dx) (y+dy) (z+dz)
            0 1 0

roll :: Camera -> Roll
roll c = r
  where (r,_,_) = camRotation c
pitch :: Camera -> Pitch
pitch c = p
  where (_,p,_) = camRotation c
yaw :: Camera -> Yaw
yaw c = y
  where (_,_,y) = camRotation c


speed :: GLdouble
speed = (pi/3.14)*7

rotSpeed :: GLdouble
rotSpeed = (pi/3.14)*1.5

scaleX :: NominalDiffTime -> Yaw -> GLdouble
scaleX dt y = (realToFrac dt) * speed * sin(y)
scaleZ :: NominalDiffTime -> Yaw -> GLdouble
scaleZ dt y = (realToFrac dt) * speed * cos(y)

checkPitch :: Camera -> Camera
checkPitch cam | pitch cam > pi/2 = cam { camRotation = (roll cam, pi/2, yaw cam) }
               | pitch cam < -pi/2 = cam { camRotation = (roll cam, -pi/2, yaw cam) }
               | otherwise = cam 

-- |The 'cameraKeyDown' function accepts the key state and modifies the camera accordingly.
cameraKeyDown :: Camera -> Key -> Camera
cameraKeyDown cam (Char 'w')               = cam { camMoving = (\(x,y,_)->(x,y,-1)) (camMoving cam)} 
cameraKeyDown cam (Char 'a')               = cam { camMoving = (\(_,y,z)->( 1,y,z)) (camMoving cam)} 
cameraKeyDown cam (Char 's')               = cam { camMoving = (\(x,y,_)->(x,y, 1)) (camMoving cam)} 
cameraKeyDown cam (Char 'd')               = cam { camMoving = (\(_,y,z)->(-1,y,z)) (camMoving cam)} 
cameraKeyDown cam (Char ' ')               = cam { camMoving = (\(x,_,z)->(x, 1,z)) (camMoving cam)} 
cameraKeyDown cam (SpecialKey KeyShiftL)   = cam { camMoving = (\(x,_,z)->(x,-1,z)) (camMoving cam)} 

cameraKeyDown cam (SpecialKey KeyUp)     = cam { camTurning = (\(r,_,y)->(r,-1,y)) (camTurning cam)}
cameraKeyDown cam (SpecialKey KeyDown)   = cam { camTurning = (\(r,_,y)->(r, 1,y)) (camTurning cam)}
cameraKeyDown cam (SpecialKey KeyLeft)   = cam { camTurning = (\(r,p,_)->(r,p,-1)) (camTurning cam)}
cameraKeyDown cam (SpecialKey KeyRight)  = cam { camTurning = (\(r,p,_)->(r,p, 1)) (camTurning cam)}
cameraKeyDown c _ = c

-- |The 'cameraKeyDown' function accepts the key state and modifies the camera accordingly.
cameraKeyUp :: Camera -> Key -> Camera
cameraKeyUp cam (Char 'w')               = cam { camMoving = (\(x,y,_)->(x,y,0)) (camMoving cam)} 
cameraKeyUp cam (Char 'a')               = cam { camMoving = (\(_,y,z)->(0,y,z)) (camMoving cam)} 
cameraKeyUp cam (Char 's')               = cam { camMoving = (\(x,y,_)->(x,y,0)) (camMoving cam)} 
cameraKeyUp cam (Char 'd')               = cam { camMoving = (\(_,y,z)->(0,y,z)) (camMoving cam)} 
cameraKeyUp cam (Char ' ')               = cam { camMoving = (\(x,_,z)->(x,0,z)) (camMoving cam)} 
cameraKeyUp cam (SpecialKey KeyShiftL)   = cam { camMoving = (\(x,_,z)->(x,0,z)) (camMoving cam)} 

cameraKeyUp cam (SpecialKey KeyUp)     = checkPitch $  cam { camTurning = (\(r,_,y)->(r,0,y)) (camTurning cam)}
cameraKeyUp cam (SpecialKey KeyDown)   = checkPitch $  cam { camTurning = (\(r,_,y)->(r,0,y)) (camTurning cam)}
cameraKeyUp cam (SpecialKey KeyLeft)   =               cam { camTurning = (\(r,p,_)->(r,p,0)) (camTurning cam)}
cameraKeyUp cam (SpecialKey KeyRight)  =               cam { camTurning = (\(r,p,_)->(r,p,0)) (camTurning cam)}
cameraKeyUp c _ = c

-- -- |The 'cameraKeyDown' function accepts the key state and modifies the camera accordingly.
-- cameraKeyDown :: Camera -> Key -> Modifiers -> Camera
-- x: 0   z: 1    (x + scaleX cam,y,z - scaleZ cam)) (camPosition cam)} 
-- x: 1   z: 0    (x - scaleZ cam,y,z - scaleX cam)) (camPosition cam)} 
-- x: 0   z: -1   (x - scaleX cam,y,z + scaleZ cam)) (camPosition cam)} 
-- x: -1  z: 0    (x + scaleZ cam,y,z + scaleX cam)) (camPosition cam)} 
-- cameraKeyDown cam (SpecialKey KeyShiftL) _ = cam { camPosition = (\(x,y,z) -> (x,y-speed,z)) (camPosition cam)} 
-- cameraKeyDown cam (Char ' ') _ = cam { camPosition = (\(x,y,z) -> (x,y+speed,z)) (camPosition cam)} 

-- cameraKeyDown cam (SpecialKey KeyUp) _    = checkPitch $ cam {pitch = (pitch cam) - rotSpeed }
-- cameraKeyDown cam (SpecialKey KeyDown) _  = checkPitch $ cam {pitch = (pitch cam) + rotSpeed }
-- cameraKeyDown cam (SpecialKey KeyLeft) _  = cam {yaw = (yaw cam) - rotSpeed }
-- cameraKeyDown cam (SpecialKey KeyRight) _ = cam {yaw = (yaw cam) + rotSpeed }
-- cameraKeyDown c _ _ = c