{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Model
Description : Reads *.obj model files and renders them.
Copyright   : (c) Jordan Medlock, 2015
Maintainer  : jordanemedlock@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Model (model) where

import qualified Graphics.Rendering.OpenGL as GL hiding (Face)
import           Graphics.Rendering.OpenGL hiding (Face)
import Data.List.Split
import Texture
import GameObject
import System.IO.Unsafe
import Data.Vector hiding ((++),forM_)
import qualified Data.Vector as V

data Face = Face { vertex :: (Int,Int,Int), 
                   texture :: (Int,Int,Int), 
                   normal :: (Int,Int,Int) }

data ModelM = ModelM { verticesM :: [Vertex3 GLdouble], 
                       normalsM :: [Normal3 GLdouble], 
                       facesM :: [Face], 
                       texVerticesM :: [TexCoord2 GLdouble], 
                       texturesM :: String }

data Model = Model { vertices :: Vector (Vertex3 GLdouble), 
                     normals :: Vector (Normal3 GLdouble), 
                     faces :: Vector (Face), 
                     texVertices :: Vector (TexCoord2 GLdouble), 
                     textures :: TextureObject }

loadModelLineSplit :: [String] -> ModelM -> ModelM
loadModelLineSplit ("v":x:y:z:_) m  = m { verticesM    = Vertex3 (read x) (read y) (read z) : verticesM m }
loadModelLineSplit ("vn":x:y:z:_) m = m { normalsM     = Normal3 (read x) (read y) (read z) : normalsM m }
loadModelLineSplit ("vt":x:y:_) m   = m { texVerticesM = TexCoord2 (read x) (1-(read y)) : texVerticesM m }
loadModelLineSplit ("f":x:y:z:_) m  = m { facesM       = Face vi ti ni : (facesM m) }
  where (vix:tix:nix:_) = splitOn "/" x
        (viy:tiy:niy:_) = splitOn "/" y
        (viz:tiz:niz:_) = splitOn "/" z
        vi = apply3 read ( vix, viy, viz)
        ti = apply3 read ( tix, tiy, tiz)
        ni = apply3 read ( nix, niy, niz)
loadModelLineSplit ("g":name:_) m = m { texturesM = name }
loadModelLineSplit xs m = unsafePerformIO (print xs >> return m)

fixModel :: ModelM -> IO Model
fixModel (ModelM v n f tv t) = do
  t' <- loadGLTextureFromFile $ ("res/models/"++t++".png")
  return $ Model (fromList v) (fromList n) (fromList f) (fromList tv) t'

modelLine :: String -> ModelM -> ModelM
modelLine s m = loadModelLineSplit (splitOn " " s) m

modelLines :: [String] -> ModelM -> ModelM 
modelLines (x:xs) m = modelLines xs (modelLine x m)
modelLines [] m = m

readModel :: String -> IO Model
readModel str = do
  fString <- readFile $ "res/models/"++str++".obj"
  let model' = modelLines (lines fString) $ ModelM [] [] [] [] ""
  fixModel model'

apply3 :: forall t t1. (t -> t1) -> (t, t, t) -> (t1, t1, t1)
apply3 f (x,y,z) = (f x, f y, f z)

indexes :: forall t1. Vector t1 -> Int -> (Int, Int, Int) -> (t1, t1, t1)
indexes xs l x = apply3 ((!) xs . (-) l ) x

renderModel :: Model -> IO ()
renderModel model' = do 
  textureBinding Texture2D $= (Just $ textures model')
  let ns = normals model'
      vs = vertices model'
      ts = texVertices model'
  V.forM_ (faces model') $ \face -> do
    color (Color3 1 1 (1 :: GLfloat))
    renderPrimitive Triangles $ do
      let (ln,lv,lt) = (V.length ns,V.length vs,V.length ts)
          n = Model.normal face
          v = Model.vertex face
          t = Model.texture face
          (n1,n2,n3) = indexes ns ln n
          (v1,v2,v3) = indexes vs lv v
          (t1,t2,t3) = indexes ts lt t

      GL.color $ Color3 1 1 (1 :: GLdouble) 
      (GL.normal n1) >> (GL.texCoord t1) >> (GL.vertex v1)
      (GL.normal n2) >> (GL.texCoord t2) >> (GL.vertex v2)
      (GL.normal n3) >> (GL.texCoord t3) >> (GL.vertex v3)
  
-- |The 'model' function accepts the short name of the model file in the res/models/ folder and they x y z position
model :: String -> X -> Y -> Z -> IO GameObject
model str x y z= do 
  m <- readModel str
  return $ SimpleObject (x,y,z) (0,0,0) $ renderModel m  