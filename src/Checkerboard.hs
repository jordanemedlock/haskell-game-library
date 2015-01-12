module Checkerboard (checkerboard) where

import Cube hiding (Width)
import GameObject hiding (X,Y,Z)
import Graphics.UI.GLUT (GLfloat, TextureObject)

type X = GLfloat
type Y = GLfloat
type Z = GLfloat

type Width = GLfloat
type Height = GLfloat
type Depth = GLfloat

cp :: X -> Y -> Z -> Bool
cp x y z = ((x'+y'+z') `mod` 2 == 0)
  where x' = floor x :: Int
        y' = floor y :: Int
        z' = floor z :: Int

checkerboard :: TextureObject -> TextureObject -> (X, Y, Z) -> (Width, Height, Depth) -> [GameObject]
checkerboard tex1 tex2 (x,y,z) (w,h,d) = output
  where smallest = minimum [w,h,d]
        (w',h',d') = (w/smallest,h/smallest,d/smallest)
        cube1 = cube tex1 (smallest/2)
        cube2 = cube tex2 (smallest/2)
        output = [(if cp x' y' z' then cube1 else cube2) {goPosition = (x+(x'*smallest),y+(y'*smallest),z+(z'*smallest))} | 
                    x' <- [-w'..w'], y' <- [-h'..h'], z' <- [-d'..d']]
