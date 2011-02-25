module Main (
  main
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data.IORef

import Stars_Display

-- | Main method.
-- Entry point of the program.
main :: IO ()
main = do
  
  (progname,args) <- getArgsAndInitialize
  initialDisplayMode $= [getBufferMode args, RGBMode, WithDepthBuffer]
  createWindow "Stars"
  
  initfn
  
  reshapeCallback $= Just (reshape)
  keyboardMouseCallback $= Just keyboardMouse
  displayCallback $= display
  visibilityCallback $= Just visible
  
  mainLoop
  where
    getBufferMode ["-sb"] = SingleBuffered
    getBufferMode _ = DoubleBuffered