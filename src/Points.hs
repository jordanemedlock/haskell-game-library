{-|
Module      : Points
Description : Contains an array of n points in a circle.
Copyright   : (c) Jordan Medlock, 2015
Maintainer  : jordanemedlock@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Points where
 
import Graphics.Rendering.OpenGL
 
-- |The 'points' function returns an array of n points in a circle.
points :: Int -> [(GLfloat,GLfloat,GLfloat)]
points n = [ (sin (2*pi*k/n'), cos (2*pi*k/n'), 0) | k <- [1..n'] ]
   where n' = fromIntegral n