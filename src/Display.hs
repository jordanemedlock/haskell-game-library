{-|
Module      : Display
Description : Holds the display function called by GLUT every time it wants to display something new
Copyright   : (c) Jordan Medlock, 2015
Maintainer  : jordanemedlock@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Display (idle, display) where
 
import Graphics.UI.GLUT hiding (position)
import Control.Monad
import Data.IORef
import World
 
-- |The 'display' function is called by GLUT and at the moment displays a bunch of cubes in a circle
display :: IORef World -> DisplayCallback
display worldRef = do 
  world <- get worldRef
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  loadIdentity
  lookThrough (camera world)
  preservingMatrix $ do
    let w = gameObjects world
    forM_ (w) $ \go -> preservingMatrix $ do
      let (x,y,z) = goPosition go
      translate $ Vector3 x y z
      color $ goColor go
      goRender go
  swapBuffers
 
-- |The 'idle' function is called by GLUT every tick
idle :: IORef World -> IdleCallback
idle worldRef = do
  world <- get worldRef
  worldRef $= woUpdate world
  postRedisplay Nothing