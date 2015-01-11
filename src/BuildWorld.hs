{-|
Module      : BuildWorld
Description : Builds the world
Copyright   : (c) Jordan Medlock, 2015
Maintainer  : jordanemedlock@gmail.com
Stability   : experimental
Portability : POSIX
-}
module BuildWorld (buildWorld) where

import World
import Points
import Cube
import Camera
import Graphics.UI.GLUT (Color3(..))

n :: Int
n = 7
-- |The 'buildWorld' function creates all of the GameObjects in the world.
-- At the moment all this does is create 7 cubes in a circle.
buildWorld :: World
buildWorld = World {
  gameObjects = map (\pos -> (cube 0.1 0) { goPosition = pos, goColor = Color3 0.5 0.5 0.5 }) (points n),
  camera      = newCamera 0 0 0
}
