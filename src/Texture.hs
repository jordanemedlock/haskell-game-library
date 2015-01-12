module Texture
   (
    loadGLTextureFromFile,

   ) where

import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import qualified Graphics.Rendering.OpenGL as GL
import Control.Applicative

loadGLTextureFromFile :: FilePath -> IO GL.TextureObject
loadGLTextureFromFile f = do t <- either error id <$> readTexture f
                             textureFilter Texture2D $= ((Linear', Nothing), Nearest)
                             texture2DWrap $= (Repeated, ClampToBorder)
                             return t