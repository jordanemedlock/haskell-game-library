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

-- |The 'Camera' structure holds all of the nessesary information to dictate 
-- where the camera should be placed and how
data Camera = Camera { x :: GLfloat, y :: GLfloat, z :: GLfloat, 
                       yaw :: GLfloat, pitch :: GLfloat, roll :: GLfloat,
                       fov :: GLfloat, aspectRatio :: GLfloat, 
                       nearClippingPlane :: GLfloat,
                       farClippingPlane :: GLfloat } deriving (Show)

-- |The 'initCamera' function creates a camera with the default values
initCamera :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Camera
initCamera x' y' z' ratio = Camera x' y' z' 90 0 0 70 ratio 0.3 1000

