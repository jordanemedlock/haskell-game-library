import Control.Monad (unless, when)
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as G
import System.Exit
import System.IO
import Texture
import Data.IORef
import Graphics.GLUtil
import qualified Data.Set as Set

main :: IO ()
main = do
    let errorCallback err description = hPutStrLn stderr description
    G.setErrorCallback (Just errorCallback)
    successfulInit <- G.init
    if not successfulInit
        then exitFailure
        else do
          mw <- G.createWindow 640 480 "Simple example, haskell style" Nothing Nothing
          case mw of Nothing -> (G.terminate >> exitFailure)
                     Just window -> do
                                    G.makeContextCurrent mw
                                    preMainLoop window
                                    G.destroyWindow window
                                    G.terminate
                                    exitSuccess



preMainLoop :: G.Window -> IO ()
preMainLoop window = do
    tex <- loadGLTextureFromFile "swirl_pattern/swirl_pattern.png"
    clearColor $= Color4 0.9 0.1243 0.2544564 1.0
    depthFunc $= Just Lequal
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    normalize $= Enabled
    texture Texture2D $= Enabled
    shadeModel $= Smooth
    mainLoop tex window


mainLoop :: TextureObject -> G.Window -> IO ()
mainLoop tex window = do
    action <- (G.windowShouldClose window)
    unless action $ do
        viewWindow window
        cal tex
        G.swapBuffers window
        G.pollEvents
        mainLoop tex window

cal tex = do
    preservingMatrix $ do
        rotate 90 (Vector3 1 0 0 :: Vector3 GLfloat)
        withTextures2D [tex] $ draw tex

draw tex = do
    textureBinding Texture2D $= Just tex
    renderPrimitive Quads $ do
    n 0 1 0
    t 0 1 >> v   1  (-1)   1
    t 1 1 >> v   1  (-1) (-1)
    t 1 0 >> v (-1) (-1) (-1)
    t 0 0 >> v (-1) (-1)   1
    where v x y z = vertex (Vertex3 x y z :: Vertex3 GLfloat)
          n x y z = normal (Normal3 x y z :: Normal3 GLfloat)
          t u v   = texCoord (TexCoord2 u v :: TexCoord2 GLfloat)

viewWindow window = do
    (width, height) <- G.getFramebufferSize window
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    clear [ColorBuffer, DepthBuffer]
    matrixMode $= Projection
    loadIdentity
    perspective 90 (fromIntegral(width)/fromIntegral(height)) 0.01 40
    matrixMode $= Modelview 0
