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

  io_rotX <- newIORef (5.0 :: GLfloat)
  io_rotY <- newIORef (-5.0 :: GLfloat)

  displayCallback $= display io_rotX io_rotY
  keyboardMouseCallback $= Just (keyboardMouse io_rotX io_rotY)
  initfn

  mainLoop