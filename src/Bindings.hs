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
 
-- |The 'reshape' function is called whenever the window gets resized
reshape :: ReshapeCallback
reshape (Size w 0) = reshape (Size w 1)
reshape size@(Size width height) = do 
  let ratio = ( fromIntegral width / fromIntegral height ) :: GLdouble
  matrixMode $= Projection
  loadIdentity
  viewport $= (Position 0 0, size)
  perspective 45.0 ratio 0.1 (100.0 :: GLdouble)
  matrixMode $= Modelview 0
  postRedisplay Nothing
 
-- |The 'keyboardMouse' function is called whenever the user hits a key or does 
-- something with the mouse
-- This one uses the keys:
-- 
--        * /space/ -> reverse direction
--        * /+/     -> increase speed
--        * /-/     -> decrease speed
--        * /left/  -> move left
--        * /right/ -> move right
--        * /up/    -> move up
--        * /down/  -> move downs
keyboardMouse :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse delta pos key Down _ _ = case key of 
  (Char ' ') -> delta $~! negate
  (Char '+') -> delta $~! (* 2)
  (Char '-') -> delta $~! (/ 2)
  (SpecialKey KeyLeft )   -> pos $~! \(x,y) -> (x-0.1,y)
  (SpecialKey KeyRight )  -> pos $~! \(x,y) -> (x+0.1,y)
  (SpecialKey KeyUp )     -> pos $~! \(x,y) -> (x,y+0.1)
  (SpecialKey KeyDown )   -> pos $~! \(x,y) -> (x,y-0.1)
  _ -> return ()
keyboardMouse _ _ _ _ _ _ = return ()

