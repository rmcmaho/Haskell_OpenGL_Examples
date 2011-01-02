module TriSelect_Display (
  
  display,
  idle,
  keyboardMouse
  
  ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Control.Concurrent (threadDelay, yield)

import Tri_Objects

zoom :: GLfloat
zoom = 1.0

ooMAXSELECT = 100

windW = 300
windH = 300

zRotation ::GLfloat
zRotation = 90.0

display :: TriObjectList -> IO ()
display objectList = do
  
  myObjectList <- get objectList
  
  preservingMatrix $ do
  
    matrixMode $= Projection
    loadIdentity
    ortho2D (-175) 175 (-175) 175
    matrixMode $= Modelview 0
  
    clear [ColorBuffer]
  
    scale zoom zoom zoom
    rotate zRotation $ Vector3 0 0 (1::GLfloat)
    
    render myObjectList
    
  swapBuffers
  flush
  
render :: [TriObject] -> IO ()
render objectList = mapM_ renderTriangle objectList
    
renderTriangle :: TriObject -> IO ()    
renderTriangle object = do
  color $ triColor object
  renderPrimitive Triangles $ do
    vertex $ v1 object
    vertex $ v2 object
    vertex $ v3 object

idle = do
  yield
  threadDelay 1000
 
keyboardMouse :: TriObjectList -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse objectList (MouseButton LeftButton) Down _ position = 
  recolorTri objectList (doSelect position)

keyboardMouse _ _ _ _ _ = return ()
--keyboardMouse objectList key state modifiers position = return ()

recolorTri :: TriObjectList -> GLint -> IO ()
recolorTri objectList index = return ()

doSelect :: Position -> GLint
doSelect (Position x y) = do
  (_, maybeHitRecords) <- do 
    vp@(_, (Size _ height)) <-  get viewport
    getHitRecords ooMAXSELECT $
      withName (Name 0) $ do
        preservingMatrix $ do
          matrixMode $= Projection
          loadIdentity
          pickMatrix (fromIntegral x, fromIntegral height - fromIntegral y) (4,4) vp
          ortho2D (-175) 175 (-175) 175
          matrixMode $= Modelview 0
          clear [ColorBuffer]
  processHits 0 maybeHitRecords

processHits :: GLint -> Maybe[HitRecord] -> GLint
processHits _ Nothing = error  "selection buffer overflow"
--processHits hits (Just buffer) =  buffer !! ((fromIntegral hits - 1) * 4 + 3)
processHits _ _ = 0::GLint


{-
-- Example of getHitRecords (eq of glSelectBuffer)


pickSceneNode :: GSRef -> KeyState -> GLint -> GLint -> STM ()
pickSceneNode gsRef dir x y=  do
      gs <- readTVar gsRef
      (_, maybeHitRecords) <- unsafeIOToSTM $ do
        vp@(_, (Size _ height)) <-  get viewport
        getHitRecords bufSize $ 
           withName (Name 0) $ do
               matrixMode $= Projection
               preservingMatrix $ do
                       loadIdentity
                       pickMatrix (fromIntegral x, fromIntegral height - fromIntegral y) (5, 5) vp
                       -- This is the same as at start of code. FIXME
                       perspective perseAngle 1.0 0.1 10000.0
                       drawCanvas'' gs Nothing
      processHits gsRef dir maybeHitRecords

processHits :: GSRef  -> KeyState -> Maybe[HitRecord] -> STM ()
processHits _ _ Nothing = error  "selection buffer overflow"
processHits gs dir (Just ((HitRecord _ _ (Name n:_)):_)) =  findHitAction gs n dir >> drawCanvas gs
processHits _ _ _ = return ()


-}