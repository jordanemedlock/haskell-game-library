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
  cameraKeyDown,
  cameraKeyUp
)
where

import GameObject
import Camera
import Data.Time.Clock

-- |The 'World' type represents the world state of the game.
data World = World { gameObjects :: [GameObject],
                     camera :: Camera }

-- |The 'woUpdate' function recursively updates all the objects stored in the world.
woUpdate :: World -> NominalDiffTime -> World
woUpdate w dt = World  { gameObjects = map (updateGo dt) (gameObjects w),
                         camera      = updateCam dt (camera w)
                       }