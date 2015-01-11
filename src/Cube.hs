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
import Graphics.UI.GLUT hiding (position, color)
 
type Width = GLdouble

{-# ANN cubeCoords "HLint: ignore" #-}
cubeCoords :: [(GLfloat, GLfloat, GLfloat)]
cubeCoords = [ ( 1, 1, 1), ( 1, 1,-1), ( 1,-1,-1), ( 1,-1, 1),
    ( 1, 1, 1), ( 1, 1,-1), (-1, 1,-1), (-1, 1, 1),
    ( 1, 1, 1), ( 1,-1, 1), (-1,-1, 1), (-1, 1, 1),
    (-1, 1, 1), (-1, 1,-1), (-1,-1,-1), (-1,-1, 1),
    ( 1,-1, 1), ( 1,-1,-1), (-1,-1,-1), (-1,-1, 1),
    ( 1, 1,-1), ( 1,-1,-1), (-1,-1,-1), (-1, 1,-1) ]

-- |The 'cube' function creates a solid cube at 0,0,0.
cube :: Width -> GOID -> GameObject
cube w i = GameObject { goPosition = (0,0,0), 
                        goColor = Color3 1 1 1, 
                        goRotation = (0,0,0), 
                        goID = i, 
                        goRender = renderObject Solid (Cube $ w*2), 
                        goUpdate = defaultUpdate  
                      }