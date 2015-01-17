{-# LANGUAGE RankNTypes #-}
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
  cameraKey,
  X,Y,Z,
  Yaw,Pitch,Roll
) where

import Graphics.UI.GLFW
import Graphics.Rendering.GLU.Raw (gluLookAt)
import Data.Time.Clock

type X = Double
type Y = Double
type Z = Double

type Yaw = Double
type Pitch = Double
type Roll = Double

lookAt :: forall a. Real a => a -> a -> a -> a -> a -> a -> a -> a -> a -> IO ()
lookAt a b c
       d e f 
       g h i = gluLookAt a' b' c' d' e' f' g' h' i'
       where [a',b',c',d',e',f',g',h',i'] = map realToFrac [a,b,c,d,e,f,g,h,i]

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

  lookAt x      y      z
         (x+dx) (y+dy) (z+dz)
         0      1      0

roll :: Camera -> Roll
roll c = r
  where (r,_,_) = camRotation c
pitch :: Camera -> Pitch
pitch c = p
  where (_,p,_) = camRotation c
yaw :: Camera -> Yaw
yaw c = y
  where (_,_,y) = camRotation c


speed :: Double
speed = (pi/3.14)*7

rotSpeed :: Double
rotSpeed = (pi/3.14)*1.5

scaleX :: NominalDiffTime -> Yaw -> Double
scaleX dt y = (realToFrac dt) * speed * sin(y)
scaleZ :: NominalDiffTime -> Yaw -> Double
scaleZ dt y = (realToFrac dt) * speed * cos(y)

checkPitch :: Camera -> Camera
checkPitch cam | pitch cam > pi/2 = cam { camRotation = (roll cam, pi/2, yaw cam) }
               | pitch cam < -pi/2 = cam { camRotation = (roll cam, -pi/2, yaw cam) }
               | otherwise = cam 

-- |The 'cameraKeyDown' function accepts the key state and modifies the camera accordingly.
cameraKey :: Camera -> Key -> KeyState -> ModifierKeys -> Camera
cameraKey cam Key'W         KeyState'Pressed _ = cam { camMoving = (\(x,y,_)->(x,y,-1)) (camMoving cam)} 
cameraKey cam Key'A         KeyState'Pressed _ = cam { camMoving = (\(_,y,z)->( 1,y,z)) (camMoving cam)} 
cameraKey cam Key'S         KeyState'Pressed _ = cam { camMoving = (\(x,y,_)->(x,y, 1)) (camMoving cam)} 
cameraKey cam Key'D         KeyState'Pressed _ = cam { camMoving = (\(_,y,z)->(-1,y,z)) (camMoving cam)} 
cameraKey cam Key'Space     KeyState'Pressed _ = cam { camMoving = (\(x,_,z)->(x, 1,z)) (camMoving cam)} 
cameraKey cam Key'LeftShift KeyState'Pressed _ = cam { camMoving = (\(x,_,z)->(x,-1,z)) (camMoving cam)} 
cameraKey cam Key'Up        KeyState'Pressed _ = cam { camTurning = (\(r,_,y)->(r,-1,y)) (camTurning cam)}
cameraKey cam Key'Down      KeyState'Pressed _ = cam { camTurning = (\(r,_,y)->(r, 1,y)) (camTurning cam)}
cameraKey cam Key'Left      KeyState'Pressed _ = cam { camTurning = (\(r,p,_)->(r,p,-1)) (camTurning cam)}
cameraKey cam Key'Right     KeyState'Pressed _ = cam { camTurning = (\(r,p,_)->(r,p, 1)) (camTurning cam)}

cameraKey cam Key'W         KeyState'Released _ = cam { camMoving = (\(x,y,_)->(x,y,0)) (camMoving cam)} 
cameraKey cam Key'A         KeyState'Released _ = cam { camMoving = (\(_,y,z)->(0,y,z)) (camMoving cam)} 
cameraKey cam Key'S         KeyState'Released _ = cam { camMoving = (\(x,y,_)->(x,y,0)) (camMoving cam)} 
cameraKey cam Key'D         KeyState'Released _ = cam { camMoving = (\(_,y,z)->(0,y,z)) (camMoving cam)} 
cameraKey cam Key'Space     KeyState'Released _ = cam { camMoving = (\(x,_,z)->(x,0,z)) (camMoving cam)} 
cameraKey cam Key'LeftShift KeyState'Released _ = cam { camMoving = (\(x,_,z)->(x,0,z)) (camMoving cam)} 
cameraKey cam Key'Up        KeyState'Released _ = cam { camTurning = (\(r,_,y)->(r,0,y)) (camTurning cam)}
cameraKey cam Key'Down      KeyState'Released _ = cam { camTurning = (\(r,_,y)->(r,0,y)) (camTurning cam)}
cameraKey cam Key'Left      KeyState'Released _ = cam { camTurning = (\(r,p,_)->(r,p,0)) (camTurning cam)}
cameraKey cam Key'Right     KeyState'Released _ = cam { camTurning = (\(r,p,_)->(r,p,0)) (camTurning cam)}
cameraKey cam _             _                 _ = cam

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