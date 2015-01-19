{-|
Module      : Main
Description : My Haskell Video Game! I still have no idea what its going to be!
Copyright   : (c) Jordan Medlock, 2015
Maintainer  : jordanemedlock@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Main (main)where

import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL
import Prelude hiding (init)
import Data.IORef 
import BuildWorld
import Bindings
import World
import Data.Time.Clock.POSIX


errorCallback :: ErrorCallback
errorCallback e s = putStrLn $ (show e) ++ " " ++ s

-- |The 'mainLoop' is called every frame and controls everything.
mainLoop :: Window -> IORef World -> IORef POSIXTime -> IO ()
mainLoop _window worldRef timeRef = do
  x <- windowShouldClose _window
  case not x of 
    False -> return ()
    True -> do

      -- Render here
      display worldRef _window
      idle worldRef timeRef _window

      swapBuffers _window

      pollEvents

      mainLoop _window worldRef timeRef


-- |The 'initGL' function initializes the GL environment and creates the window.
initGL :: IO (Window)
initGL = do 
  True <- init
  _monitor <- getPrimaryMonitor 
  (Just _window) <- createWindow 1000 1000 "Hello World!" Nothing Nothing
  makeContextCurrent $ Just _window

  depthFunc           $= Just Less -- the comparison function for depth the buffer
  shadeModel          $= Smooth
  lighting            $= Enabled
  clearColor          $= Color4 0 0 0 0
  position (Light 0)  $= Vertex4 5 5 10 0
  lightModelAmbient   $= Color4 1 1 1 1
  light (Light 0)     $= Enabled
  diffuse (Light 0)   $= Color4 1 1 1 1
  blend               $= Enabled
  blendFunc           $= (SrcAlpha, OneMinusSrcAlpha)
  colorMaterial       $= Just (FrontAndBack, AmbientAndDiffuse)
  texture Texture2D   $= Enabled
  normalize           $= Enabled
  cullFace            $= Nothing

  return _window

-- |The 'initCallbacks' function adds all the GLFW callbacks.
initCallbacks :: Window -> IORef World -> IO ()
initCallbacks _window worldRef = do

  setCursorInputMode _window CursorInputMode'Disabled

  setErrorCallback $ Just errorCallback

  setKeyCallback _window $ Just $ keyboard worldRef

  setWindowSizeCallback _window $ Just reshape 

  setCursorPosCallback _window $ Just (mouse worldRef)

  return ()

-- |The 'main' function runs the whole goddamn thing!
main :: IO ()
main = do

  _window <- initGL

  world <- buildWorld
  worldRef <- newIORef world
  time <- getPOSIXTime
  timeRef <- newIORef time

  initCallbacks _window worldRef

  mainLoop _window worldRef timeRef

  terminate
