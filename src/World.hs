module World (
  World(..),
  woUpdate,
  GameObject(..),
  Camera(..),
  lookThrough,
  cameraKeys
)
where

import GameObject
import Camera

data World = World { gameObjects :: [GameObject],
                     camera :: Camera }

woUpdate :: World -> World
woUpdate w = World { gameObjects = map updateGo (gameObjects w),
                     camera      = updateCam (camera w)
                   }