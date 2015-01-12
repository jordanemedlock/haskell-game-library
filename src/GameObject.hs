{-|
Module      : GameObject
Description : The representation of a object used by the game
Copyright   : (c) Jordan Medlock, 2015
Maintainer  : jordanemedlock@gmail.com
Stability   : experimental
Portability : POSIX
-}
module GameObject (
  GameObject(..),
  defaultUpdate,
  updateGo,
  GOID,X,Y,Z
) where

import Graphics.UI.GLUT

type GOID = Int
type X = GLfloat
type Y = GLfloat
type Z = GLfloat

-- |The 'GameObject' type stores all the information needed to draw an object to
-- the GLUT context.
data GameObject 
  -- |The constructor 'SimpleObject' provides only the minimal information 
  -- to draw the objects.
  = SimpleObject { goPosition :: (X,Y,Z),
                   goRotation :: (X,Y,Z),
                   goRender :: IO ()
                 } | 
  -- |The constructor 'ComplexObject' provides additional callbacks for update,
  -- keyboard and mouse
   ComplexObject { goPosition :: (X,Y,Z),
                   goRotation :: (X,Y,Z),
                   goRender :: IO (),
                   goUpdate :: GameObject -> GameObject,
                   goKeyboard :: GameObject -> Key -> Modifiers -> GameObject,
                   goMouse :: GameObject -> MouseButton -> KeyState -> Position -> GameObject
                 }
                 
-- |The 'defaultUpdate' function is the simplest update you can give a game object.
-- It is equivalent to 'id'.
defaultUpdate :: GameObject -> GameObject
defaultUpdate go = go

-- |The 'updateGo' function is a convenience function which turns the goUpdate 
-- function into one that only requires one 'GameObject' argument.
updateGo :: GameObject -> GameObject
updateGo go@(SimpleObject _ _ _ ) = go
updateGo go@(ComplexObject _ _ _ u _ _) = u go