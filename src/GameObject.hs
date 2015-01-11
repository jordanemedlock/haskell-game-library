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
data GameObject = GameObject { goPosition :: (X,Y,Z),
                               goRotation :: (X,Y,Z),
                               goColor :: Color3 GLfloat,
                               goID :: GOID, 
                               goRender :: IO (),
                               goUpdate :: GameObject -> GameObject
                             }
-- |The 'defaultUpdate' function is the simplest update you can give a game object.
-- It is equivalent to 'id'.
defaultUpdate :: GameObject -> GameObject
defaultUpdate go = go

-- |The 'updateGo' function is a convenience function which turns the goUpdate 
-- function into one that only requires one 'GameObject' argument.
updateGo :: GameObject -> GameObject
updateGo go = goUpdate go go