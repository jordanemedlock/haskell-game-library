{-|
Module      : Bindings
Description : Holds all the bindings for the GLUT callbacks
Copyright   : (c) Jordan Medlock, 2015
Maintainer  : jordanemedlock@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Bindings (idle, display, reshape, keyboardMouse) where

import Data.IORef
import Graphics.UI.GLUT
import Display
import World
 
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
 
-- |The 'keyboardMouse' function is called whenever the user hits a key or does 
-- something with the mouse.
-- This function passes off control to the camera so that it can move.
keyboardMouse :: IORef World -> KeyboardMouseCallback
keyboardMouse _ (Char '\27') Down _ _ = exit -- ESC
keyboardMouse worldRef key Down mods _ = do
  world <- get worldRef
  let newWorld = world { camera = cameraKeys (camera world) key mods }
  worldRef $= newWorld
  return ()
keyboardMouse _ _ _ _ _ = return ()


