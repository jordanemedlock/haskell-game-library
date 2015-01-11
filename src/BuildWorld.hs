module BuildWorld (buildWorld) where

import World
import Points
import Cube
import Camera
import Graphics.UI.GLUT (Color3(..))

n :: Int
n = 7

buildWorld :: World
buildWorld = World {
  gameObjects = map (\pos -> (cube 0.1 0) { goPosition = pos, goColor = Color3 0.5 0.5 0.5 }) (points n),
  camera      = newCamera 0 0 0
}
