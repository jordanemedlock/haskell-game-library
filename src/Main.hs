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
import Prelude hiding (init)
import Data.IORef 
import BuildWorld
import World

errorCallback :: ErrorCallback
errorCallback e s = putStrLn $ (show e) ++ " " ++ s


mainLoop :: Window -> IO ()
mainLoop _window = do
  x <- windowShouldClose _window
  case not x of 
    False -> return ()
    True -> do

      -- Render here

      swapBuffers _window

      pollEvents

      mainLoop _window


-- |The 'initGL' function initializes the GL environment and creates the window.
initGL :: IO (Window)
initGL = do 
  True <- init
  _monitor <- getPrimaryMonitor 
  (Just _window) <- createWindow 1000 1000 "Hello World!" _monitor Nothing
  makeContextCurrent $ Just _window
  return _window

-- |The 'initCallbacks' function adds all the GLUT callbacks.
initCallbacks :: IORef World -> IO ()
initCallbacks worldRef = do

  setErrorCallback $ Just errorCallback

  

  return ()

-- |The 'main' function runs the whole goddamn thing!
main :: IO ()
main = do
  world <- buildWorld
  worldRef <- newIORef world


  _window <- initGL

  initCallbacks worldRef

  mainLoop _window

  terminate
