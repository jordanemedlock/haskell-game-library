{-|
Module      : BuildWorld
Description : Builds the world.
Copyright   : (c) Jordan Medlock, 2015
Maintainer  : jordanemedlock@gmail.com
Stability   : experimental
Portability : POSIX
-}
module BuildWorld (buildWorld) where

import World
import Camera
import Model
import Box
import Texture
-- |The 'buildWorld' function creates all of the GameObjects in the world.
-- At the moment all this does is create a /possibly/ 10x10 checkerboard floor.
buildWorld :: IO World
buildWorld = do
  m <- model "guyblue" 0 5 0
  tex <- loadGLTextureFromFile "res/textures/brick1.png"
  return $ World {
    gameObjects = [m,m,m,m],
    camera      = newCamera 0 5 0
  }
