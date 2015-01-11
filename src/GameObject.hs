{-|
Module      : GameObject
Description : The representation of a object used by the game
Copyright   : (c) Jordan Medlock, 2015
Maintainer  : jordanemedlock@gmail.com
Stability   : experimental
Portability : POSIX
-}
module GameObject where

import Graphics.UI.GLUT

data GameObject = GameObject { goPosition :: (GLfloat,GLfloat,GLfloat),
                               goRotation :: (GLfloat,GLfloat,GLfloat),
                               goColor :: Color3 GLfloat,
                               goID :: Int, 
                               goRender :: IO (),
                               goUpdate :: GameObject -> GameObject
                             }
defaultUpdate :: GameObject -> GameObject
defaultUpdate go = go

updateGo :: GameObject -> GameObject
updateGo go = goUpdate go go