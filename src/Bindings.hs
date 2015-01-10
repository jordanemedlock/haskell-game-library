module Bindings (idle, display, reshape, keyboardMouse) where

import Data.IORef
import Graphics.UI.GLUT
import Display
 
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

