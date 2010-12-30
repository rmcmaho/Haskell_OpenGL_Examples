module Main (
  main
  
  ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Graphics.UI.GLUT.Menu

import System.Exit

import TriSelect_Display (display, idle)

exitLoop = exitSuccess

main = do
  (progname,_) <- getArgsAndInitialize
  initialDisplayMode $= [RGBMode, DoubleBuffered]
  createWindow "Select Test"
  
  reshapeCallback $= Nothing
  keyboardMouseCallback $= Nothing
  displayCallback $= display
  idleCallback $= Just display
  
  -- This does not work with "runhaskell". It needs to be compiled with the C wrapper.
  attachMenu RightButton (Menu [MenuEntry "Exit" exitLoop])
  
  mainLoop