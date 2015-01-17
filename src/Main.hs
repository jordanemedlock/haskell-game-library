{-|
Module      : Main
Description : My Haskell Video Game! I still have no idea what its going to be!
Copyright   : (c) Jordan Medlock, 2015
Maintainer  : jordanemedlock@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Main where

import Graphics.UI.GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Prelude hiding (init)
import Data.IORef 
import BuildWorld
import Bindings
import World
import Data.Time.Clock.POSIX


errorCallback :: ErrorCallback
errorCallback e s = putStrLn $ (show e) ++ " " ++ s


mainLoop :: Window -> IORef World -> IORef POSIXTime -> IO ()
mainLoop _window worldRef timeRef = do
  x <- windowShouldClose _window
  case not x of 
    False -> return ()
    True -> do

      -- Render here
      display worldRef
      idle worldRef timeRef

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

  GL.position (GL.Light 0) GL.$= GL.Vertex4 5 5 10 0
  GL.light    (GL.Light 0) GL.$= GL.Enabled
  GL.lighting   GL.$= GL.Enabled
  GL.cullFace   GL.$= Just GL.Back
  GL.depthFunc  GL.$= Just GL.Less
  GL.clearColor GL.$= GL.Color4 0.05 0.05 0.05 1
  GL.normalize  GL.$= GL.Enabled

  return _window

-- |The 'initCallbacks' function adds all the GLUT callbacks.
initCallbacks :: Window -> IORef World -> IO ()
initCallbacks _window worldRef = do

  setErrorCallback $ Just errorCallback

  setKeyCallback _window $ Just $ keyboard worldRef

  setWindowSizeCallback _window $ Just reshape 

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
