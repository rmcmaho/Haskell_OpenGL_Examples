module TriSelect_Display (
  
  display,
  idle,
  keyboardMouse
  
  ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import GHC.Conc (unsafeIOToSTM)

import Control.Concurrent (threadDelay, yield)

import Random

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
  -- Log
  --print myObjectList

  preservingMatrix $ do
    matrixMode $= Projection
    loadIdentity
    ortho2D (-175) 175 (-175) 175
    matrixMode $= Modelview 0
    clear [ColorBuffer]
    scale zoom zoom zoom
    rotate zRotation $ Vector3 0 0 (1::GLfloat)
    render Render myObjectList
  swapBuffers
  flush
  
render :: RenderMode -> [TriObject] -> IO ()
render Render objectList = mapM_ renderTriangle objectList
render Select objectList = renderAndLoadTriangles objectList (fromIntegral (length objectList) - 1)
    
renderAndLoadTriangles :: [TriObject] -> GLuint -> IO ()  
renderAndLoadTriangles (object:[]) name = do
  loadName (Name name)
  renderTriangle object
renderAndLoadTriangles (object:objectList) name = do
  loadName (Name name)
  print (Name name)
  print object
  renderTriangle object
  renderAndLoadTriangles objectList (name-1)
  
renderTriangle :: TriObject -> IO ()    
renderTriangle object = do
  color $ triColor object
  renderPrimitive Triangles $ do
    vertex $ v1 object
    vertex $ v2 object
    vertex $ v3 object

idle = postRedisplay Nothing
 
keyboardMouse :: TriObjectList -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse objectList (MouseButton LeftButton) Down _ position = do
  myObjectList <- get objectList
  print myObjectList
  selectedItem <- doSelect position myObjectList
  gen <- newStdGen
  objectList $= recolorTri gen myObjectList (convertNameToIndex (length myObjectList) selectedItem)

keyboardMouse objectList (MouseButton MiddleButton) Down _ position = do
  myObjectList <- get objectList
  print myObjectList
  selectedItem <- doSelect position myObjectList
  objectList $= growTri myObjectList (convertNameToIndex (length myObjectList) selectedItem)

keyboardMouse _ _ _ _ _ = return ()
--keyboardMouse objectList key state modifiers position = return ()

convertNameToIndex _ (-1) = -1
convertNameToIndex objectLength index = fromIntegral objectLength - index - 1

recolorTri :: StdGen -> [TriObject] -> GLint -> [TriObject]
recolorTri _ objectList (-1) = objectList
recolorTri gen objectList index = listHead ++ return newObject ++ listTail
  where oldObject = objectList !! fromIntegral index
        c1:c2:c3:[] = take 3 $ randomRs (0,100) gen
        newObject = TriObject (v1 oldObject) (v2 oldObject) (v3 oldObject) (Color3 ((c1 + 50) / 150.0) ((c2 + 50) / 150.0) ((c3 + 50) / 150.0) )
        listHead = take (fromIntegral index) objectList
        listTail = drop (fromIntegral index + 1) objectList

growTri :: [TriObject] -> GLint -> [TriObject]
growTri objectList (-1) = objectList
growTri objectList index = listHead ++ return newObject ++ listTail
  where oldObject = objectList !! fromIntegral index
        newObject = growTriangle oldObject
        listHead = take (fromIntegral index) objectList
        listTail = drop (fromIntegral index + 1) objectList


growTriangle (TriObject (Vertex2 v0_X v0_Y) (Vertex2 v1_X v1_Y) (Vertex2 v2_X v2_Y) color0) = TriObject newV0 newV1 newV2 color0
  where v@(Vertex2 v_X v_Y) = Vertex2 ((v0_X + v1_X + v2_X) / 3) ((v0_Y + v1_Y + v2_Y) / 3) 
        newV0 = Vertex2 (1.5 * (v0_X - v_X) + v_X) (1.5 * (v0_Y - v_Y) + v_Y) 
        newV1 = Vertex2 (1.5 * (v1_X - v_X) + v_X) (1.5 * (v1_Y - v_Y) + v_Y) 
        newV2 = Vertex2 (1.5 * (v2_X - v_X) + v_X) (1.5 * (v2_Y - v_Y) + v_Y) 

doSelect :: Position -> [TriObject] -> IO GLint
doSelect pos@(Position x y) myObjectList= do
  maybeHitRecords <- getTriangleSelects pos myObjectList
  print maybeHitRecords
  return (getHeadRecord maybeHitRecords)
--doSelect _ = return (0::GLint)

getHeadRecord :: Maybe[HitRecord] -> GLint
getHeadRecord Nothing = -1
getHeadRecord (Just []) = -1
getHeadRecord (Just (HitRecord _ _ (Name n:_):_)) = fromIntegral n
getHeadRecord _ = -1

{-
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
-}


getTriangleSelects :: Position -> [TriObject] -> IO (Maybe[HitRecord])
getTriangleSelects (Position x y) myObjectList = do
  vp@(_, Size _ height) <-  get viewport
  (_, maybeHitRecords) <- getHitRecords ooMAXSELECT $ withName (Name 0) $ preservingMatrix $ do
      matrixMode $= Projection
      loadIdentity
      pickMatrix (fromIntegral x, fromIntegral height - fromIntegral y) (4,4) vp
      ortho2D (-175) 175 (-175) 175
      matrixMode $= Modelview 0
      clear [ColorBuffer]
      scale zoom zoom zoom
      rotate zRotation $ Vector3 0 0 (1::GLfloat)
      render Select myObjectList
  return maybeHitRecords


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