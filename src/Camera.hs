module Camera where

import Graphics.UI.GLUT

data Camera = Camera { x :: GLfloat, y :: GLfloat, z :: GLfloat, 
                       yaw :: GLfloat, pitch :: GLfloat, roll :: GLfloat,
                       fov :: GLfloat, aspectRatio :: GLfloat, 
                       nearClippingPlane :: GLfloat,
                       farClippingPlane :: GLfloat } deriving (Show)

initCamera :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Camera
initCamera x' y' z' ratio = Camera x' y' z' 90 0 0 70 ratio 0.3 1000

