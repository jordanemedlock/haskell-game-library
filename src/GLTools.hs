module GLTools where

import Graphics.UI.GLUT

drawNormal3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
drawNormal3f x y z = normal $ Normal3 x y z
drawVertex3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
drawVertex3f x y z = vertex $ Vertex3 x y z
texCoord2f :: GLfloat -> GLfloat -> IO ()
texCoord2f u v = texCoord $ TexCoord2 u v
