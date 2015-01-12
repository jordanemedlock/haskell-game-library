{-|
Module      : Texture
Description : Provides a utility method for reading texture files
Copyright   : (c) Jordan Medlock, 2015
Maintainer  : jordanemedlock@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Texture
   (
    loadGLTextureFromFile,

   ) where

import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import qualified Graphics.Rendering.OpenGL as GL
import Control.Applicative

-- |The 'loadGLTextureFromFile' function does exactly what it says it does.
loadGLTextureFromFile :: FilePath -> IO GL.TextureObject
loadGLTextureFromFile f = do t <- either error id <$> readTexture f
                             textureFilter Texture2D $= ((Linear', Nothing), Nearest)
                             texture2DWrap $= (Repeated, ClampToBorder)
                             return t