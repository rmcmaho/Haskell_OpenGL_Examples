module Main (
  main
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data.IORef

import Stars_Display
import Star_Rec

-- | Main method.
-- Entry point of the program.
main :: IO ()
main = do
  
  (progname,args) <- getArgsAndInitialize
  initialDisplayMode $= [getBufferMode args, RGBMode, WithDepthBuffer]
  createWindow "Stars"
  
  starFlag <- newIORef NormalStars
  starList <- newIORef []
  
  initfn starFlag starList
  
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse starFlag)
  displayCallback $= display starList
  visibilityCallback $= Just (visible starFlag starList)
  
  mainLoop
  where
    getBufferMode ["-sb"] = SingleBuffered
    getBufferMode _ = DoubleBuffered