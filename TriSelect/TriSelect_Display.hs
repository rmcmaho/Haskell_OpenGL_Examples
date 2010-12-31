module TriSelect_Display (
  
  display,
  idle
  
  ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Control.Concurrent (threadDelay, yield)

import Tri_Objects

zoom :: GLfloat
zoom = 1.0

zRotation = 90.0

display :: TriObjectList -> IO ()
display objectList = do
  
  preservingMatrix $ do
  
    matrixMode $= Projection
    loadIdentity
    ortho2D (-175) 175 (-175) 175
    matrixMode $= Modelview 0
  
    clear [ColorBuffer]
  
    scale zoom zoom zoom
    rotate zRotation $ Vector3 0 0 (1::GLfloat)
    
    myObjectList <- get objectList
    render myObjectList
    
  swapBuffers
  flush
  
render :: [TriObject] -> IO ()
render objectList = mapM_ renderTriangle objectList
    
renderTriangle :: TriObject -> IO ()    
renderTriangle object = do
  renderPrimitive Triangles $ do
    vertex $ v1 object
    vertex $ v2 object
    vertex $ v3 object

idle = do
  yield
  threadDelay 1000