{-
Module      : Camera
Description : Holds information to store and modify the camera for the game
Copyright   : (c) Jordan Medlock 2015
Maintainer  : jordanemedlock@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Camera where

import Graphics.UI.GLUT
import Graphics.Rendering.GLU.Raw (gluLookAt)


-- |The 'Camera' structure holds all of the nessesary information to dictate 
-- where the camera should be placed and how
data Camera = Camera { camPosition :: (GLdouble, GLdouble, GLdouble),
                       yaw :: GLfloat, pitch :: GLfloat, roll :: GLfloat,
                       fov :: GLfloat, 
                       nearClippingPlane :: GLfloat,
                       farClippingPlane :: GLfloat } deriving (Show)

-- |The 'newCamera' function creates a camera with the default values
newCamera :: GLdouble -> GLdouble -> GLdouble -> Camera
newCamera x y z = Camera (x,y,z) 0 0 0 70 0.3 1000

updateCam :: Camera -> Camera
updateCam c = c

lookThrough :: Camera -> IO ()
lookThrough c = do
  let (x,y,z) = camPosition c
  gluLookAt 0 0 10
            0 0 0 
            0 1 0

  rotate (pitch c) $ Vector3 1 0 0
  rotate (yaw c) $ Vector3 0 1 0 
  rotate (roll c) $ Vector3 0 0 1
  translate $ Vector3 (-x) (-y) (-z)

speed :: GLdouble
speed = 1

cameraKeys :: Camera -> Key -> Modifiers -> Camera
cameraKeys cam (Char 'w') _ = cam { camPosition = (\(x,y,z) -> (x,y,z-speed)) (camPosition cam)} 
cameraKeys cam (Char 'a') _ = cam { camPosition = (\(x,y,z) -> (x-speed,y,z)) (camPosition cam)} 
cameraKeys cam (Char 's') _ = cam { camPosition = (\(x,y,z) -> (x,y,z+speed)) (camPosition cam)} 
cameraKeys cam (Char 'd') _ = cam { camPosition = (\(x,y,z) -> (x+speed,y,z)) (camPosition cam)} 
cameraKeys c _ _ = c