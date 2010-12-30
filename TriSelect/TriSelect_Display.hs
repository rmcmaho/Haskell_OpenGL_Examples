module TriSelect_Display (
  
  display,
  idle
  
  ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT (swapBuffers, flush)

import Control.Concurrent (threadDelay, yield)

display = do
  yield
  clear [ColorBuffer,DepthBuffer]
  threadDelay 100
  swapBuffers
  flush
  
idle = do
  yield
  threadDelay 1000