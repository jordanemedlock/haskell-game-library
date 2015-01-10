module Display (idle, display) where
 
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Cube
import Points
import Graphics.Rendering.GLU.Raw (gluLookAt)
 
display :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> DisplayCallback
display angle pos = do 
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  loadIdentity
  gluLookAt 0 0 10
            0 0 0 
            0 1 0
  (x',y') <- get pos
  translate $ Vector3 x' y' 0
  preservingMatrix $ do
    a <- get angle
    rotate a $ Vector3 0 1 1
    translate $ Vector3 0 0 (0 :: GLfloat)
    forM_ (points 7) $ \(x,y,z) -> preservingMatrix $ do
      color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
      translate $ Vector3 (x*3) (y*3) (z*3)
      cube 0.3
  swapBuffers
 
idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
  d <- get delta
  angle $~! (+ d)
  postRedisplay Nothing