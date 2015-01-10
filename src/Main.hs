import Graphics.UI.GLUT
import Bindings
import Data.IORef

-- |The 'initGL' function initializes the GL environment and creates the window
initGL :: IO ()
initGL = do 
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  windowSize          $= (Size 1000 1000)
  initialDisplayMode  $= [WithDepthBuffer, DoubleBuffered, RGBMode]
  depthFunc           $= Just Less -- the comparison function for depth the buffer
  shadeModel          $= Smooth
  depthFunc           $= Just Less -- the comparison function for depth the buffer
  lighting            $= Enabled
  lightModelAmbient   $= Color4 0.5 0.5 0.5 1
  light (Light 0)     $= Enabled
  diffuse (Light 0)   $= Color4 1 1 1 1
  blend               $= Enabled
  blendFunc           $= (SrcAlpha, OneMinusSrcAlpha)
  colorMaterial       $= Just (FrontAndBack, AmbientAndDiffuse)

-- |The 'initCallbacks' function adds all the GLUT callbacks
initCallbacks :: IO ()
initCallbacks = do
  angle <- newIORef 0.0
  delta <- newIORef 1
  pos <- newIORef (0.0,0.0)

  displayCallback       $= display angle pos
  idleCallback          $= Just (idle angle delta)
  keyboardMouseCallback $= Just (keyboardMouse delta pos)
  reshapeCallback       $= Just reshape

-- |The 'main' function runs the whole goddamn thing!
main :: IO ()
main = do
  initGL

  initCallbacks

  mainLoop