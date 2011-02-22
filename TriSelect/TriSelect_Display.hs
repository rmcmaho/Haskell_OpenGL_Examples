{-# OPTIONS_HADDOCK ignore-exports #-}
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

-- | Sets the zoom scale of everything displayed.
-- Increase to make everything bigger. Decrease to make everything smaller.
zoom :: GLfloat
zoom = 1.0

ooMAXSELECT = 100

-- | Sets the angle around the Z-axis everything is rotated.
-- Could be used to make the display spin(?).
zRotation ::GLfloat
zRotation = 90.0

-- | Display callback
display :: TriObjectList -- ^ List of TriObject to display
           -> IO ()
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
    render Render myObjectList
  swapBuffers
  flush
  
-- | Renders the triangles. If 'RenderMode' is 'Render', then the triangles are simply rendered.
-- If 'RenderMode' is 'Select', then the triangle names are loaded before rendering the triangles. 
render :: RenderMode -- ^ 'Select' or 'Render' 
          -> [TriObject] -- ^ List of TriObject to render
          -> IO ()
render Render objectList = mapM_ renderTriangle objectList
render Select objectList = renderAndLoadTriangles objectList (fromIntegral (length objectList) - 1)
    
-- | Recursively loads and renders all TriObject in the list.
renderAndLoadTriangles :: [TriObject] -- ^ List of remaining TriObject to load and render
                          -> GLuint -- ^ Name of the TriObject at the head of the list
                          -> IO () 
renderAndLoadTriangles (object:[]) name = do
  loadName (Name name)
  renderTriangle object
renderAndLoadTriangles (object:objectList) name = do
  loadName (Name name)
  renderTriangle object
  renderAndLoadTriangles objectList (name-1)
  
-- | Renders the given TriObject
renderTriangle :: TriObject -> IO ()    
renderTriangle object = do
  color $ triColor object
  renderPrimitive Triangles $ do
    vertex $ v1 object
    vertex $ v2 object
    vertex $ v3 object

-- | Idle callback.
-- Runs whenever OpenGL has nothing else to do.
idle :: IO ()
idle = postRedisplay Nothing


-- | Keyboard and mouse callback.
-- Invoked when any keyboard or mouse button changes state (Up->Down or Down->Up)
keyboardMouse :: TriObjectList -- ^List of TriObject to manipulate
                 -> Key -- ^Key activated (LeftButton, MiddleButton, etc)
                 -> KeyState -- ^Up or Down
                 -> Modifiers -- ^Modifier keys (Ctrl, Alt, Shift, etc)
                 -> Position -- ^Position of the mouse on the screen
                 -> IO () -- ^IO monad to wrap

keyboardMouse objectList (MouseButton LeftButton) Down _ position = do
  myObjectList <- get objectList
  selectedItem <- doSelect position myObjectList
  gen <- newStdGen
  objectList $= recolorTri gen myObjectList (convertNameToIndex (length myObjectList) selectedItem)

keyboardMouse objectList (MouseButton MiddleButton) Down _ position = do
  myObjectList <- get objectList
  selectedItem <- doSelect position myObjectList
  objectList $= growTri myObjectList (convertNameToIndex (length myObjectList) selectedItem)

keyboardMouse _ _ _ _ _ = return ()

-- | Objects are loaded and named in reverse order.
-- This method converts a name to an index position.
convertNameToIndex :: (Num b, Integral a)
                      => a -- ^ Number of objects 
                      -> b -- ^ Name of the object
                      -> b -- ^ Index of the object
convertNameToIndex _ (-1) = -1
convertNameToIndex objectLength name = fromIntegral objectLength - name - 1

-- | Generates a new color for the TriObject at the specified index.
recolorTri :: StdGen -- ^ Random number generator
              -> [TriObject] -- ^ List of TriObject
              -> GLint -- ^ Index of TriObject to change
              -> [TriObject] -- ^ Original list but with the specified TriObject recolored
recolorTri _ objectList (-1) = objectList
recolorTri gen objectList index = listHead ++ return newObject ++ listTail
  where oldObject = objectList !! fromIntegral index
        c1:c2:c3:[] = take 3 $ randomRs (0,100) gen
        newObject = TriObject (v1 oldObject) (v2 oldObject) (v3 oldObject) (Color3 ((c1 + 50) / 150.0) ((c2 + 50) / 150.0) ((c3 + 50) / 150.0) )
        listHead = take (fromIntegral index) objectList
        listTail = drop (fromIntegral index + 1) objectList

-- | Grows the TriObject at the specified index.
growTri :: [TriObject] -- ^ List of TriObject
           -> GLint -- ^ Index of TriObject ot grow
           -> [TriObject] -- ^ Original list but with the specified TriObject bigger
growTri objectList (-1) = objectList
growTri objectList index = listHead ++ return newObject ++ listTail
  where oldObject = objectList !! fromIntegral index
        newObject = growTriangle oldObject
        listHead = take (fromIntegral index) objectList
        listTail = drop (fromIntegral index + 1) objectList

-- | Takes a TriObject, grows it, and returns the new/bigger TriObject
growTriangle :: TriObject -- ^ Original TriObject
                -> TriObject -- ^ New/bigger TriObject
growTriangle (TriObject (Vertex2 v0_X v0_Y) (Vertex2 v1_X v1_Y) (Vertex2 v2_X v2_Y) color0) = TriObject newV0 newV1 newV2 color0
  where v@(Vertex2 v_X v_Y) = Vertex2 ((v0_X + v1_X + v2_X) / 3) ((v0_Y + v1_Y + v2_Y) / 3) 
        newV0 = Vertex2 (1.5 * (v0_X - v_X) + v_X) (1.5 * (v0_Y - v_Y) + v_Y) 
        newV1 = Vertex2 (1.5 * (v1_X - v_X) + v_X) (1.5 * (v1_Y - v_Y) + v_Y) 
        newV2 = Vertex2 (1.5 * (v2_X - v_X) + v_X) (1.5 * (v2_Y - v_Y) + v_Y) 

-- | Get the name of the 'TriObject' at the current cursor location.
doSelect :: Position -- ^ Current position of the cursor
            -> [TriObject] -- ^ List of TriObject
            -> IO GLint -- ^ Name of the TriObject under the cursor
doSelect pos@(Position x y) myObjectList= do
  maybeHitRecords <- getTriangleSelects pos myObjectList
  return (getHeadRecord maybeHitRecords)

-- | Return the name of the first 'HitRecord' if more than one is generated.
getHeadRecord :: Maybe[HitRecord] -- ^ List of HitRecord
                 -> GLint -- ^ Name of the HitRecord
getHeadRecord Nothing = -1
getHeadRecord (Just []) = -1
getHeadRecord (Just (HitRecord _ _ (Name n:_):_)) = fromIntegral n
getHeadRecord _ = -1

-- | Discovers which triangle, if any, the cursor is currently pointing at.
getTriangleSelects :: Position -- ^ Current position of cursor
                      -> [TriObject] -- ^ List of TriObject
                      -> IO (Maybe[HitRecord]) -- ^ Possible list of HitRecord
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