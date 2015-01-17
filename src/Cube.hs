{-|
Module      : Cube
Description : Creates GLUT cubes.
Copyright   : (c) Jordan Medlock, 2015
Maintainer  : jordanemedlock@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Cube (cube,Width) where

import GameObject
import Graphics.Rendering.OpenGL hiding (position)
import Box

data Direction = DFront | DBack | DLeft | DRight | DUp | DDown deriving (Show,Eq)

type Width = GLfloat

-- |The 'cube' function creates a solid cube at 0,0,0.
cube :: TextureObject -> Width -> GameObject
cube t w = box t w w w