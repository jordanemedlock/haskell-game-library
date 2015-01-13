{-|
Module      : Main
Description : My Haskell Video Game! I still have no idea what its going to be!
Copyright   : (c) Jordan Medlock, 2015
Maintainer  : jordanemedlock@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Main where

import Graphics.UI.GLUT
import Bindings
import Data.IORef
import BuildWorld
import Data.Time.Clock.POSIX

-- |The 'initGL' function initializes the GL environment and creates the window.
initGL :: IO ()
initGL = do 
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  windowSize          $= (Size 1000 1000)
  initialDisplayMode  $= [WithDepthBuffer, DoubleBuffered]
  depthFunc           $= Just Less -- the comparison function for depth the buffer
  shadeModel          $= Smooth
  lighting            $= Enabled
  clearColor          $= Color4 0 0 0 0
  lightModelAmbient   $= Color4 1 1 1 1
  light (Light 0)     $= Enabled
  diffuse (Light 0)   $= Color4 1 1 1 1
  blend               $= Enabled
  blendFunc           $= (SrcAlpha, OneMinusSrcAlpha)
  colorMaterial       $= Just (FrontAndBack, AmbientAndDiffuse)
  texture Texture2D   $= Enabled
  normalize           $= Enabled
  cullFace            $= Nothing
  globalKeyRepeat     $= GlobalKeyRepeatOn

-- |The 'initCallbacks' function adds all the GLUT callbacks.
initCallbacks :: IO ()
initCallbacks = do
  w <- buildWorld
  world <- newIORef w
  t <- getPOSIXTime
  time <- newIORef t

  displayCallback       $= display world
  idleCallback          $= Just (idle world time)
  keyboardMouseCallback $= Just (keyboardMouse world)
  reshapeCallback       $= Just reshape

-- |The 'main' function runs the whole goddamn thing!
main :: IO ()
main = do
  initGL

  initCallbacks

  mainLoop