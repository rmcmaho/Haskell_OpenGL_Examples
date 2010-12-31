module Main (
  main
  
  ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

-- For exit menu
import Graphics.UI.GLUT.Menu
import System.Exit

-- For callbacks
import TriSelect_Display (display, idle)

-- For trianlge objects
import Tri_Objects

-- Random number generation
import Random

-- To use stateful objects (naughty)
import Data.IORef

-- Main
main = do
  (progname,_) <- getArgsAndInitialize
  initialDisplayMode $= [RGBMode, DoubleBuffered]
  createWindow "Select Test"
  
  gen <- newStdGen
  let numObjects = 10
  objectList <- newIORef (initObjects gen numObjects)
  
  reshapeCallback $= Nothing
  keyboardMouseCallback $= Nothing
  displayCallback $= display
  idleCallback $= Just display
  
  -- This does not work with "runhaskell". It needs to be compiled.
  attachMenu RightButton (Menu [MenuEntry "Exit" exitSuccess])
  
  mainLoop
  
-- Initialize triangle objects  
initObjects :: StdGen -> Int -> [TriObject]
initObjects gen numObjects  
  | numObjects > 0 = (randomObject gen1):(initObjects gen2 (numObjects-1))
  | otherwise = []
  where
    (gen1, gen2) = split gen