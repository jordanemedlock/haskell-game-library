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
import Graphics.UI.GLUT hiding (position)
 
data Direction = DFront | DBack | DLeft | DRight | DUp | DDown deriving (Show,Eq)

type Width = GLfloat

drawNormal3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
drawNormal3f x y z = normal $ Normal3 x y z
drawVertex3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
drawVertex3f x y z = vertex $ Vertex3 x y z
texCoord2f :: GLfloat -> GLfloat -> IO ()
texCoord2f u v = texCoord $ TexCoord2 u v

{-# ANN cubeCoords "HLint: ignore" #-}
cubeCoords :: [(GLfloat, GLfloat, GLfloat)]
cubeCoords = [ ( 1, 1, 1), ( 1, 1,-1), ( 1,-1,-1), ( 1,-1, 1),
    ( 1, 1, 1), ( 1, 1,-1), (-1, 1,-1), (-1, 1, 1),
    ( 1, 1, 1), ( 1,-1, 1), (-1,-1, 1), (-1, 1, 1),
    (-1, 1, 1), (-1, 1,-1), (-1,-1,-1), (-1,-1, 1),
    ( 1,-1, 1), ( 1,-1,-1), (-1,-1,-1), (-1,-1, 1),
    ( 1, 1,-1), ( 1,-1,-1), (-1,-1,-1), (-1, 1,-1) ]


drawCube :: GLfloat -> TextureObject -> IO ()
drawCube w tex = do
  textureBinding Texture2D $= Just tex
  color (Color3 1 1 (1 :: GLfloat))
  renderPrimitive Quads $ do
    -- Front
    drawNormal3f 0 0 (-w)
    texCoord2f 0 1 >> drawVertex3f (-w) (-w)  w
    texCoord2f 1 1 >> drawVertex3f w    (-w)  w
    texCoord2f 1 0 >> drawVertex3f w      w   w
    texCoord2f 0 0 >> drawVertex3f (-w)   w   w
    -- Back
    drawNormal3f 0 0 (w)
    texCoord2f 0 1 >> drawVertex3f w    (-w) (-w)
    texCoord2f 1 1 >> drawVertex3f (-w) (-w) (-w)
    texCoord2f 1 0 >> drawVertex3f (-w)   w  (-w)
    texCoord2f 0 0 >> drawVertex3f w      w  (-w)
    -- Right
    drawNormal3f (-w) 0 0
    texCoord2f 0 1 >> drawVertex3f w (-w)   w
    texCoord2f 1 1 >> drawVertex3f w (-w) (-w)
    texCoord2f 1 0 >> drawVertex3f w w    (-w)
    texCoord2f 0 0 >> drawVertex3f w w      w
    -- Left
    drawNormal3f (w) 0 0
    texCoord2f 0 1 >> drawVertex3f (-w) (-w)  (-w)
    texCoord2f 1 1 >> drawVertex3f (-w) (-w)  w
    texCoord2f 1 0 >> drawVertex3f (-w) w     w
    texCoord2f 0 0 >> drawVertex3f (-w) w     (-w)
    -- Top
    drawNormal3f 0 (-w) 0
    texCoord2f 0 1 >> drawVertex3f (-w) w   w
    texCoord2f 1 1 >> drawVertex3f w    w   w
    texCoord2f 1 0 >> drawVertex3f w    w (-w)
    texCoord2f 0 0 >> drawVertex3f (-w) w (-w)
    -- Bottom
    drawNormal3f 0 (w) 0
    texCoord2f 0 1 >> drawVertex3f (-w) (-w) (-w)
    texCoord2f 1 1 >> drawVertex3f w    (-w) (-w)
    texCoord2f 1 0 >> drawVertex3f w    (-w)   w
    texCoord2f 0 0 >> drawVertex3f (-w) (-w)   w

-- |The 'cube' function creates a solid cube at 0,0,0.
cube :: Width -> GOID -> TextureObject -> GameObject
cube w i t = GameObject { goPosition = (0,0,0), 
                          goColor = Color3 1 1 1, 
                          goRotation = (0,0,0), 
                          goID = i, 
                          goRender = drawCube w t, 
                          goUpdate = defaultUpdate  
                        }