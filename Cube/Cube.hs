module Main (
  main
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Cube_Display

-- | Main method.
-- Entry point of the program.
main :: IO ()
main = do
  (progname,_) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
  createWindow "red 3D lighted cube"
  displayCallback $= display
  initfn
  
  mainLoop