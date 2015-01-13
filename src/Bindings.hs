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
  keyboardDown,
  keyboardUp,
  keyboardMouse,
  specialDown,
  specialUp
) where

import Data.IORef
import Graphics.UI.GLUT
import Display
import World
import Data.Time.Clock.POSIX
 
debug :: (Show a) => a -> IO ()
debug x = do
  print x

-- |The 'reshape' function is called whenever the window gets resized.
reshape :: ReshapeCallback
reshape (Size w 0) = reshape (Size w 1)
reshape size@(Size width height) = do 
  let ratio = ( fromIntegral width / fromIntegral height ) :: GLdouble
  matrixMode $= Projection
  loadIdentity
  viewport $= (Position 0 0, size)
  perspective 40.0 ratio 0.1 (100.0 :: GLdouble)
  matrixMode $= Modelview 0
  postRedisplay Nothing
 
-- |The 'keyboardDown' function is called whenever the user presses a key.
-- This function passes off control to the camera so that it can move.
keyboardDown :: IORef World -> KeyboardCallback
keyboardDown _ '\27' _ = exit -- ESC
keyboardDown worldRef key _ = do
  world <- get worldRef
  let newWorld = world { camera = cameraKeyDown (camera world) (Char key) }
  worldRef $= newWorld
  return ()

-- |The 'keyboardDown' function is called whenever the user presses a key.
-- This function passes off control to the camera so that it can move.
specialDown :: IORef World -> SpecialCallback
specialDown worldRef key _ = do
  -- debug key
  world <- get worldRef
  let newWorld = world { camera = cameraKeyDown (camera world) (SpecialKey key) }
  worldRef $= newWorld
  return ()

-- |The 'keyboardDown' function is called whenever the user presses a key.
-- This function passes off control to the camera so that it can move.
keyboardUp :: IORef World -> KeyboardCallback
keyboardUp worldRef key _ = do
  world <- get worldRef
  let newWorld = world { camera = cameraKeyUp (camera world) (Char key) }
  worldRef $= newWorld
  return ()

-- |The 'keyboardUp' function is called whenever the user presses a key.
-- This function passes off control to the camera so that it can move.
specialUp :: IORef World -> SpecialCallback
specialUp worldRef key _ = do
  world <- get worldRef
  let newWorld = world { camera = cameraKeyUp (camera world) (SpecialKey key) }
  worldRef $= newWorld
  return ()

keyboardMouse :: IORef World -> KeyboardMouseCallback
keyboardMouse worldRef key state (Modifiers _ _ _) mouse = case state of
  Down -> case key of  
    (Char k) -> keyboardDown worldRef k mouse
    (SpecialKey k) -> specialDown worldRef k mouse
    (MouseButton _) -> return () -- TODO add mouse callback
  Up -> case key of 
    (Char k) -> keyboardUp worldRef k mouse
    (SpecialKey k) -> specialUp worldRef k mouse
    (MouseButton _) -> return () -- TODO add mouse callback




-- |The 'idle' function is called by GLUT every frame.
idle :: IORef World -> IORef POSIXTime -> IdleCallback
idle worldRef timeRef = do
  newTime <- getPOSIXTime
  oldTime <- get timeRef
  let dt = newTime - oldTime
  debug (1/(realToFrac dt) :: GLdouble)
  timeRef $= newTime
  world <- get worldRef
  worldRef $= woUpdate world dt
  postRedisplay Nothing