{-|
Module      : Bindings
Description : Holds all the bindings for the GLUT callbacks
Copyright   : (c) Jordan Medlock, 2015
Maintainer  : jordanemedlock@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Bindings (
  idle, 
  display, 
  reshape, 
  keyboard,
  mouse
) where

import Data.IORef
import Display
import World
import Graphics.UI.GLFW
import Data.Time.Clock.POSIX
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL.GL.StateVar 
import System.Exit

-- debug :: (Show a) => a -> IO ()
-- debug x = do
--   print x

-- |The 'reshape' function is called whenever the window gets resized.
reshape :: WindowSizeCallback
reshape win w 0 = reshape win w 1
reshape _   width height = do 
  glViewport 0 0 (fromIntegral width*2) (fromIntegral height*2)
 
-- |The 'keyboardDown' function is called whenever the user presses a key.
-- This function passes off control to the camera so that it can move.
keyboard :: IORef World -> KeyCallback
keyboard _ _ Key'Escape _ KeyState'Pressed _ = terminate >> exitSuccess
keyboard worldRef _ key _ s m = do
  world <- get worldRef
  let newWorld = world { camera = cameraKey (camera world) key s m }
  worldRef $= newWorld
  return ()





-- |The 'idle' function is called by GLUT every frame.
idle :: IORef World -> IORef POSIXTime -> Window -> IO ()
idle worldRef timeRef win = do
  newTime <- getPOSIXTime
  oldTime <- get timeRef
  let dt = newTime - oldTime
  setWindowTitle win $ show $ (floor $ 1/dt :: Int)
  -- debug (1/(realToFrac dt) :: GLdouble)
  timeRef $= newTime
  world <- get worldRef
  worldRef $= woUpdate world dt

mouse :: IORef World -> CursorPosCallback
mouse worldRef win x y = do
  world <- get worldRef
  let (c, newY) = cameraMouse (camera world) x y
      newWorld = world { camera = c }
  worldRef $= newWorld
  setCursorPos win x newY
