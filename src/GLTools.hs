{-|
Module      : GLTools
Description : A set of tools to make my life easier.
Copyright   : (c) Jordan Medlock, 2015
Maintainer  : jordanemedlock@gmail.com
Stability   : experimental
Portability : POSIX
-}
module GLTools where

import Graphics.Rendering.OpenGL

drawNormal3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
drawNormal3f x y z = normal $ Normal3 x y z
drawVertex3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
drawVertex3f x y z = vertex $ Vertex3 x y z
texCoord2f :: GLfloat -> GLfloat -> IO ()
texCoord2f u v = texCoord $ TexCoord2 u v
