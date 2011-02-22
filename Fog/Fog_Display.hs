{-# OPTIONS_HADDOCK ignore-exports #-}
module Fog_Display (

display,
initfn

) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

data TriObject = TriObject {v1 :: Vertex3 GLfloat,
                            v2 :: Vertex3 GLfloat,
                            v3 :: Vertex3 GLfloat,
                            n1 :: Normal3 GLfloat,
                            n2 :: Normal3 GLfloat,
                            n3 :: Normal3 GLfloat
                            } deriving (Show)

                            
triangleList = [TriObject {v1 = Vertex3 1.0 0.0 (0.0 :: GLfloat),
                           v2 = Vertex3 1.0 0.0 (5.0 :: GLfloat),
                           v3 = Vertex3 0.707107 0.707107 (0.0 :: GLfloat),
                           n1 = Normal3 1.0 0.0 (0.0 :: GLfloat),
                           n2 = Normal3 1.0 0.0 (0.0 :: GLfloat),
                           n3 = Normal3 0.707107 0.707107 (0.0 :: GLfloat)},
                TriObject {v1 = Vertex3 0.707107 0.707107 (5.0 :: GLfloat),
                           v2 = Vertex3 0.0 1.0 (0.0 :: GLfloat),
                           v3 = Vertex3 0.0 1.0 (5.0 :: GLfloat),
                           n1 = Normal3 0.707107 0.707107 (0.0 :: GLfloat),
                           n2 = Normal3 0.0 1.0 (0.0 :: GLfloat),
                           n3 = Normal3 0.0 1.0 (0.0 :: GLfloat)},
                TriObject {v1 = Vertex3 (-0.707107) 0.707107 (0.0 :: GLfloat),
                           v2 = Vertex3 (-0.707107) 0.707107 (5.0 :: GLfloat),
                           v3 = Vertex3 (-1.0) 0.0 (0.0 :: GLfloat),
                           n1 = Normal3 (-0.707107) 0.707107 (0.0 :: GLfloat),
                           n2 = Normal3 (-0.707107) 0.707107 (0.0 :: GLfloat),
                           n3 = Normal3 (-1.0) 0.0 (0.0 :: GLfloat)},
                TriObject {v1 = Vertex3 (-1.0) 0.0 (5.0 :: GLfloat),
                           v2 = Vertex3 (-0.707107) (-0.707107) (0.0 :: GLfloat),
                           v3 = Vertex3 (-0.707107) (-0.707107) (5.0 :: GLfloat),
                           n1 = Normal3 (-1.0) 0.0 (0.0 :: GLfloat),
                           n2 = Normal3 (-0.707107) (-0.707107) (0.0 :: GLfloat),
                           n3 = Normal3 (-0.707107) (-0.707107) (0.0 :: GLfloat)},
                TriObject {v1 = Vertex3 0.0 (-1.0) (0.0 :: GLfloat),
                           v2 = Vertex3 0.0 (-1.0) (5.0 :: GLfloat),
                           v3 = Vertex3 0.707107 (-0.707107) (0.0 :: GLfloat),
                           n1 = Normal3 0.0 (-1.0) (0.0 :: GLfloat),
                           n2 = Normal3 0.0 (-1.0) (0.0 :: GLfloat),
                           n3 = Normal3 0.707107 (-0.707107) (0.0 :: GLfloat)},
                TriObject {v1 = Vertex3 0.707107 (-0.707107) (5.0 :: GLfloat),
                           v2 = Vertex3 1.0 0.0 (0.0 :: GLfloat),
                           v3 = Vertex3 1.0 0.0 (5.0 :: GLfloat),
                           n1 = Normal3 0.707107 (-0.707107) (0.0 :: GLfloat),
                           n2 = Normal3 1.0 0.0 (0.0 :: GLfloat),
                           n3 = Normal3 1.0 0.0 (0.0 :: GLfloat)}]


-- | Display callback.
display :: IO ()
display = do
  clear [ColorBuffer, DepthBuffer]
  preservingMatrix $ do
    translate $ zTranslate
    rotate rotY $ Vector3 0.0 1.0 0.0
    rotate rotX $ Vector3 1.0 0.0 0.0
    scale 1.0 1.0 (10.0::GLfloat)
    
--    renderTube triangleList
    
    callList cubeList
  
  swapBuffers
  flush
  where
    zTranslate = Vector3 0.0 0.0 (-65.0::GLfloat)
    rotY = -5.0 :: GLfloat
    rotX = 5.0 :: GLfloat
    cubeList = DisplayList 1

renderTube = mapM_ renderSection

renderSection triangleObject = 
  renderPrimitive TriangleStrip $ do
    normal $ n1 triangleObject
    vertex $ v1 triangleObject
    normal $ n2 triangleObject
    vertex $ v2 triangleObject
    normal $ n3 triangleObject
    vertex $ v3 triangleObject

initfn :: IO ()
initfn = do
  frontFace $= CW
  depthFunc $= Just Lequal

  
  ambient light0 $= lightAmbient
  diffuse light0 $= lightDiffuse
  position light0 $= lightPosition
  
  lightModelAmbient $= modelAmb
  lightModelTwoSide $= Enabled
  
  lighting $= Enabled
  light light0 $= Enabled
  
  materialShininess Front $= matShin_front
  materialSpecular Front $= matSpec_front
  materialDiffuse Front $= matDiff_front
  
  materialShininess Back $= matShin_back
  materialSpecular Back $= matSpec_back
  materialDiffuse Back $= matDiff_back

  fog $= Enabled
  fogMode $= Exp fogDensity
  fogColor $= fog_color
  clearColor $= Color4 0.8 0.8 0.8 1.0

  defineList cubeList Compile $ renderTube triangleList

  matrixMode $= Projection
  perspective 45.0 1.0 1.0 200.0
  matrixMode $= Modelview 0

  where
    light0 = Light 0
    lightAmbient = Color4 0.1 0.1 0.1 1.0
    lightDiffuse = Color4 1.0 1.0 1.0 1.0
    lightPosition = Vertex4 90.0 90.0 0.0 0.0
    modelAmb = Color4 0.0 0.0 0.0 1.0
    matShin_front = 30.0
    matSpec_front = Color4 0.0 0.0 0.0 1.0
    matDiff_front = Color4 0.0 1.0 0.0 1.0
    matShin_back = 50.0
    matSpec_back = Color4 0.0 0.0 1.0 1.0
    matDiff_back = Color4 1.0 0.0 0.0 1.0
    fogDensity = 0.02
    fog_color = Color4 0.8 0.8 0.8 1.0
    cubeList = DisplayList 1
    
