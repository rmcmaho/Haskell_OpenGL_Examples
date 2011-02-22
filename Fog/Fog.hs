module Main (
  main
  ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data.IORef

import Fog_Display

-- | Main method.
-- Entry point of the program.
main :: IO ()
main = do
  
  (progname,_) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
  createWindow "Fog test"
	
  displayCallback $= display
  initfn

  mainLoop