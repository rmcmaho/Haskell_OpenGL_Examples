module TriSelect_Display (
  
  display,
  idle
  
  ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Control.Concurrent (threadDelay, yield)

zoom :: GLfloat
zoom = 1.0

zRotation = 90.0

display = do
  
  preservingMatrix $ do
  
    matrixMode $= Projection
    loadIdentity
    ortho2D (-175) 175 (-175) 175
    matrixMode $= Modelview 0
  
    clear [ColorBuffer]
  
    scale zoom zoom zoom
    rotate zRotation $ Vector3 0 0 (1::GLfloat)
    
    render
    
  swapBuffers
  flush
  
render = return ()

idle = do
  yield
  threadDelay 1000