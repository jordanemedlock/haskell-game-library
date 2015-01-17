{-|
Module      : Box
Description : A rectangular prism.
Copyright   : (c) Jordan Medlock, 2015
Maintainer  : jordanemedlock@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Box (box) where

import GameObject
import Graphics.Rendering.OpenGL hiding (Height)
import GLTools

type Width = GLfloat
type Height = GLfloat
type Depth = GLfloat


drawBox :: Width -> Height -> Depth -> TextureObject -> IO ()
drawBox w h d tex = do
  textureBinding Texture2D $= Just tex
  color (Color3 1 1 (1 :: GLfloat))
  renderPrimitive Quads $ do
    -- Front
    drawNormal3f 0 0 (-d)
    texCoord2f 0 1 >> drawVertex3f (-w) (-h)  d
    texCoord2f 1 1 >> drawVertex3f   w  (-h)  d
    texCoord2f 1 0 >> drawVertex3f   w    h   d
    texCoord2f 0 0 >> drawVertex3f (-w)   h   d
    -- Back
    drawNormal3f 0 0 (w)
    texCoord2f 0 1 >> drawVertex3f   w  (-h) (-d)
    texCoord2f 1 1 >> drawVertex3f (-w) (-h) (-d)
    texCoord2f 1 0 >> drawVertex3f (-w)   h  (-d)
    texCoord2f 0 0 >> drawVertex3f   w    h  (-d)
    -- Right
    drawNormal3f (-w) 0 0
    texCoord2f 0 1 >> drawVertex3f w (-h)   d
    texCoord2f 1 1 >> drawVertex3f w (-h) (-d)
    texCoord2f 1 0 >> drawVertex3f w   h  (-d)
    texCoord2f 0 0 >> drawVertex3f w   h    d
    -- Left
    drawNormal3f (w) 0 0
    texCoord2f 0 1 >> drawVertex3f (-w) (-h) (-d)
    texCoord2f 1 1 >> drawVertex3f (-w) (-h)   d
    texCoord2f 1 0 >> drawVertex3f (-w)   h    d
    texCoord2f 0 0 >> drawVertex3f (-w)   h  (-d)
    -- Top
    drawNormal3f 0 (-w) 0
    texCoord2f 0 1 >> drawVertex3f (-w) h   d
    texCoord2f 1 1 >> drawVertex3f   w  h   d
    texCoord2f 1 0 >> drawVertex3f   w  h (-d)
    texCoord2f 0 0 >> drawVertex3f (-w) h (-d)
    -- Bottom
    drawNormal3f 0 (w) 0
    texCoord2f 0 1 >> drawVertex3f (-w) (-h) (-d)
    texCoord2f 1 1 >> drawVertex3f   w  (-h) (-d)
    texCoord2f 1 0 >> drawVertex3f   w  (-h)   d
    texCoord2f 0 0 >> drawVertex3f (-w) (-h)   d

-- |The 'box' function creates a box shape at (0,0,0).
box :: TextureObject -> Width -> Height -> Depth -> GameObject
box t w h d = SimpleObject {
  goPosition = (0,0,0),
  goRotation = (0,0,0),
  goRender = drawBox w h d t
}