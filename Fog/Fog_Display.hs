{-# OPTIONS_HADDOCK ignore-exports #-}
module Fog_Display (

display,
initfn,
keyboardMouse

) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data.IORef

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
display :: IORef (GLfloat) -- ^ Current rotation about the x-axis
           -> IORef (GLfloat) -- ^ Current rotation about the y-axis
           -> IO ()
display io_rotX io_rotY = do
  clear [ColorBuffer, DepthBuffer]
  
  rotY <- get io_rotY
  rotX <- get io_rotX
  
  preservingMatrix $ do
    translate $ zTranslate
    rotate rotY $ Vector3 0.0 1.0 0.0
    rotate rotX $ Vector3 1.0 0.0 0.0
    scale 1.0 1.0 (10.0::GLfloat)
    callList cubeList
  swapBuffers
  flush
  where
    zTranslate = Vector3 0.0 0.0 (-65.0::GLfloat)
    cubeList = DisplayList 1

-- | Keyboard and mouse callback.
-- Invoked when any keyboard or mouse button changes state (Up->Down or Down->Up)
keyboardMouse :: IORef (GLfloat) -- ^ Current rotation about the x-axis
                 -> IORef (GLfloat) -- ^ Current rotation about the y-axis
                 -> Key -- ^Key activated (LeftButton, MiddleButton, etc)
                 -> KeyState -- ^Up or Down
                 -> Modifiers -- ^Modifier keys (Ctrl, Alt, Shift, etc)
                 -> Position -- ^Position of the mouse on the screen
                 -> IO () -- ^IO monad to wrap

keyboardMouse _ _ (Char 'd') Down _ _ = do
  (Exp currentFog) <- get fogMode
  fogMode $= Exp (currentFog * 1.10)
  postRedisplay Nothing
  
keyboardMouse _ _ (Char 'D') Down _ _ = do
  (Exp currentFog) <- get fogMode
  fogMode $= Exp (currentFog / 1.10)
  postRedisplay Nothing
  
keyboardMouse io_rotX _ (SpecialKey KeyUp) Down _ _ = do   
  rotX <- get io_rotX
  io_rotX $= rotX - 5.0
  postRedisplay Nothing
  
keyboardMouse io_rotX _ (SpecialKey KeyDown) Down _ _ = do   
  rotX <- get io_rotX
  io_rotX $= rotX + 5.0
  postRedisplay Nothing

keyboardMouse _ io_rotY (SpecialKey KeyLeft) Down _ _ = do   
  rotY <- get io_rotY
  io_rotY $= rotY - 5.0
  postRedisplay Nothing

keyboardMouse _ io_rotY (SpecialKey KeyRight) Down _ _ = do   
  rotY <- get io_rotY
  io_rotY $= rotY + 5.0
  postRedisplay Nothing

keyboardMouse _ _ _ _ _ _ = return ()

-- | Initializes various things for the program.
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

  defineList cubeList Compile $ renderTube

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
    renderTube = renderPrimitive TriangleStrip $ mapM_ renderSection triangleList
    renderSection triangleObject = do
      normal $ n1 triangleObject
      vertex $ v1 triangleObject
      normal $ n2 triangleObject
      vertex $ v2 triangleObject
      normal $ n3 triangleObject
      vertex $ v3 triangleObject
    
