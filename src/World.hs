{-|
Module      : World
Description : The game world.
Copyright   : (c) Jordan Medlock, 2015
Maintainer  : jordanemedlock@gmail.com
Stability   : experimental
Portability : POSIX
-}
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

-- |The 'World' type represents the world state of the game.
data World = World { gameObjects :: [GameObject],
                     camera :: Camera }

-- |The 'woUpdate' function recursively updates all the objects stored in the world.
woUpdate :: World -> World
woUpdate w = World { gameObjects = map updateGo (gameObjects w),
                     camera      = updateCam (camera w)
                   }