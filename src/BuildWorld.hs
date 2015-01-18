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
-- import Checkerboard
import Camera
-- import Texture
import Model

-- |The 'buildWorld' function creates all of the GameObjects in the world.
-- At the moment all this does is create a /possibly/ 10x10 checkerboard floor.
buildWorld :: IO World
buildWorld = do
  -- tex1 <- loadGLTextureFromFile "res/textures/brick1.png"
  -- tex2 <- loadGLTextureFromFile "res/textures/brick2.png"
  m <- model "guyblue" 0 0 0 
  return $ World {
    gameObjects = [m],
    camera      = newCamera 0 5 0
  }
