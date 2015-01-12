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
  cameraKeys,
  X,Y,Z,
  Yaw,Pitch,Roll
) where

import Graphics.UI.GLUT
import Graphics.Rendering.GLU.Raw (gluLookAt)

type X = GLdouble
type Y = GLdouble
type Z = GLdouble

type Yaw = GLfloat
type Pitch = GLfloat
type Roll = GLfloat


-- |The 'Camera' structure holds all of the necessary information to dictate 
-- where the camera should be placed and how.
data Camera = Camera { camPosition :: (X, Y, Z),
                       yaw :: Yaw, pitch :: Pitch, roll :: Roll
                     }

-- |The 'newCamera' function creates a camera with the default values.
newCamera :: X -> Y -> Z -> Camera
newCamera x y z = Camera (x,y,z) 0 0 0

-- |The 'updateCam' function should be called every idle frame. At the moment 
-- it doesn't do anything and is equivalent to 'id'.
updateCam :: Camera -> Camera
updateCam c = c

-- |The 'lookThrough' function uses the cameras position to move the 3D environment
-- to the cameras perspective.
lookThrough :: Camera -> IO ()
lookThrough c = do
  let (x,y,z) = camPosition c
  gluLookAt 0 0 10
            0 0 0 
            0 1 0

  rotate (pitch c) $ Vector3 1 0 0
  rotate (yaw c)   $ Vector3 0 1 0 
  rotate (roll c)  $ Vector3 0 0 1
  translate $ Vector3 (-x) (-y) (-z)

speed :: GLdouble
speed = 1

-- |The 'cameraKeys' function accepts the key state and modifies the camera accordingly
cameraKeys :: Camera -> Key -> Modifiers -> Camera
cameraKeys cam (Char 'w') _ = cam { camPosition = (\(x,y,z) -> (x,y,z-speed)) (camPosition cam)} 
cameraKeys cam (Char 'a') _ = cam { camPosition = (\(x,y,z) -> (x-speed,y,z)) (camPosition cam)} 
cameraKeys cam (Char 's') _ = cam { camPosition = (\(x,y,z) -> (x,y,z+speed)) (camPosition cam)} 
cameraKeys cam (Char 'd') _ = cam { camPosition = (\(x,y,z) -> (x+speed,y,z)) (camPosition cam)} 
cameraKeys cam (SpecialKey KeyShiftL) _ = cam { camPosition = (\(x,y,z) -> (x,y-speed,z)) (camPosition cam)} 
cameraKeys cam (Char ' ') _ = cam { camPosition = (\(x,y,z) -> (x,y+speed,z)) (camPosition cam)} 
cameraKeys c _ _ = c