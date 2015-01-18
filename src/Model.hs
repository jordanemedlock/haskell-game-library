module Model where

import qualified Graphics.Rendering.OpenGL as GL hiding (Face)
import Data.List.Split
import Texture
import Control.Monad
import GameObject
import System.IO.Unsafe

data Face a = Face { vertex :: GL.Vertex3 a, texture :: GL.Vertex3 a, normal :: GL.Normal3 a }

data Model a = Model { vertices :: [GL.Vertex3 a], normals :: [GL.Normal3 a], faces :: [Face a], texVertices :: [GL.TexCoord2 a], textures :: Either String GL.TextureObject }

debug :: (Show a) => a -> a
debug x = unsafePerformIO $ print x >> return x

loadModelLineSplit :: (Read a) => [String] -> Model a -> Model a
loadModelLineSplit ("v":x:y:z:_) m  = m { vertices    = GL.Vertex3 (read x) (read y) (read z) : vertices m }
loadModelLineSplit ("vn":x:y:z:_) m = m { normals     = GL.Normal3 (read x) (read y) (read z) : normals m }
loadModelLineSplit ("vt":x:y:_) m   = m { texVertices = GL.TexCoord2 (read x) (read y) : texVertices m }
loadModelLineSplit ("f":x:y:z:_) m  = m { faces       = Face vi ti ni : (faces m) }
  where (vix:tix:nix:_) = splitOn "/" x
        (viy:tiy:niy:_) = splitOn "/" y
        (viz:tiz:niz:_) = splitOn "/" z
        vi = GL.Vertex3 (read vix) (read viy) (read viz)
        ti = GL.Vertex3 (read tix) (read tiy) (read tiz)
        ni = GL.Normal3 (read nix) (read niy) (read niz)
loadModelLineSplit ("g":name:_) m = m { textures = Left name }
loadModelLineSplit xs m = unsafePerformIO (print xs >> return m)

modelLine :: (Read a) => String -> Model a -> Model a
modelLine s m = loadModelLineSplit (splitOn " " s) m

modelLines :: (Read a) => [String] -> Model a -> Model a
modelLines (x:xs) m = modelLines xs (modelLine x m)
modelLines [] m = m

readModel :: (Read a) => String -> IO (Model a)
readModel str = do
  fString <- readFile $ "res/models/"++str++".obj"
  let model' = modelLines (lines fString) $ Model [] [] [] [] (Left "")
  t <- loadGLTextureFromFile $ ("res/models/"++(either id (const "fail") (textures model'))++".png")
  return model' { textures = Right t }

maybeRight :: Either a b -> Maybe b
maybeRight (Left _) = Nothing
maybeRight (Right x) = Just x

renderModel :: (RealFrac a, GL.NormalComponent a, GL.VertexComponent a, GL.TexCoordComponent a, Show a) => Model a -> IO ()
renderModel model' = do 
  GL.textureBinding GL.Texture2D GL.$= maybeRight (textures model')
  let ns = normals model'
      vs = vertices model'
      ts = texVertices model'
      _  = debug $ length ns
      _  = debug $ length vs
      _  = debug $ length ts
  forM_ (faces model') $ \face -> do
    GL.color (GL.Color3 1 1 (1 :: GL.GLfloat))
    GL.renderPrimitive GL.Triangles $ do
      let GL.Normal3 nx ny nz = debug $ normal face
          (n1,n2,n3) = (ns !! (floor nx), ns !! (floor ny), ns !! (floor nz))
          GL.Vertex3 vx vy vz = debug $ vertex face
          (v1,v2,v3) = (vs !! (floor vx), vs !! (floor vy), vs !! (floor vz))
          GL.Vertex3 tx ty tz = debug $ texture face
          (t1,t2,t3) = (ts !! (floor tx), ts !! (floor ty), ts !! (floor tz))
      GL.normal n1
      GL.texCoord t1
      GL.vertex v1
      GL.normal n2
      GL.texCoord t2
      GL.vertex v2
      GL.normal n3
      GL.texCoord t3
      GL.vertex v3
  
model :: String -> X -> Y -> Z -> IO GameObject
model str x y z= do 
  m <- readModel str
  return $ SimpleObject (x,y,z) (0,0,0) $ renderModel (m :: Model GL.GLdouble) 