{-|
Module      : Display
Description : Holds the display function called by GLUT every frame.
Copyright   : (c) Jordan Medlock, 2015
Maintainer  : jordanemedlock@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Display (display) where
 
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL
import Control.Monad
import Data.IORef
import World
 
-- |The 'display' function displays the world.
display :: IORef World -> Window -> IO ()
display worldRef _window = do 
  world <- get worldRef
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  loadIdentity
  (w,h) <- getWindowSize _window
  lookThrough (camera world) w h
  preservingMatrix $ do
    let gos = gameObjects world
    forM_ (gos) $ \go -> preservingMatrix $ do
      let (x,y,z) = goPosition go
      translate $ Vector3 x y z
      goRender go
 