module Cube (cube, cubeFrame) where
 
import Graphics.UI.GLUT
 
vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

normal3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
normal3f (x, y, z) = normal $ Normal3 x y z

vertexWithNormal :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertexWithNormal xyz = do
  vertex3f xyz
  normal3f xyz

applyTo3Tuple :: (a -> b) -> (a, a, a) -> (b, b, b)
applyTo3Tuple f (x,y,z) = (f x, f y, f z)

cubeCoords :: [(GLfloat, GLfloat, GLfloat)]
cubeCoords = [ ( 1, 1, 1), ( 1, 1,-1), ( 1,-1,-1), ( 1,-1, 1),
    ( 1, 1, 1), ( 1, 1,-1), (-1, 1,-1), (-1, 1, 1),
    ( 1, 1, 1), ( 1,-1, 1), (-1,-1, 1), (-1, 1, 1),
    (-1, 1, 1), (-1, 1,-1), (-1,-1,-1), (-1,-1, 1),
    ( 1,-1, 1), ( 1,-1,-1), (-1,-1,-1), (-1,-1, 1),
    ( 1, 1,-1), ( 1,-1,-1), (-1,-1,-1), (-1, 1,-1) ]

cubeFrame :: GLfloat -> IO ()
cubeFrame w = renderPrimitive Lines $ mapM_ (vertexWithNormal . applyTo3Tuple (*w)) cubeCoords

cube :: GLdouble -> IO ()
cube w = renderObject Solid (Cube $ w*2)