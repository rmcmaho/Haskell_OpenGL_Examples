module Stars_Display (
  reshape,
  keyboardMouse,
  display,
  visible,
  initfn
  ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data.IORef
import Random

import Star_Rec

reshape :: Size -> IO ()
reshape (Size width height) = do
  viewport $= (Position 0 0, Size width height)
  matrixMode $= Projection
  loadIdentity
  ortho2D (-0.5) (fromIntegral width+0.5) (-0.5) (fromIntegral height+0.5)
  matrixMode $= Modelview 0

keyboardMouse :: Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse _ _ _ _ = return ()

display :: IO ()
display = return ()

visible :: Visibility -> IO ()
visible _ = return ()

-- | Creates a new random star
newStarList :: StdGen -- ^ Random number generator
               -> StarFlag -- ^ Flag for stars
               -> GLint -- ^ Z-axis Offset
               -> GLint -- ^ Number of stars to make
               -> [StarRec] -- ^ List of new stars
newStarList gen flag starDepth numStars
  | numStars > 0 = newStar gen1 flag starDepth:newStarList gen2 flag starDepth (numStars - 1)
  | otherwise = []
  where
    (gen1, gen2) = split gen
    
newStar :: StdGen
           -> StarFlag
           -> GLint
           -> StarRec
newStar gen flag starDepth = StarRec {starType = getType randomType, x = (starX, starX),
                                   y = (starY, starY), z = (starZ, starZ),
                                   offsetX = newOffsetX, offsetY = newOffsetY,
                                   offsetZ = newOffsetZ, rotation = 0.0}
                          where
                            -- Generate type
                            (randomType, g1) = randomR (0,4) gen
                            getType :: Int -> StarType
                            getType 0 = Circle
                            getType _ = Streak
                            -- Get position
                            (starX, g2) = randomR (0 - (maxPos / 2.0), maxPos / 2.0) g1
                            (starY, g3) = randomR (0 - (maxPos / 2), maxPos / 2) g2
                            (starZ, g4) = randomR (fromIntegral starDepth, maxPos + fromIntegral starDepth) g3
                            -- Get offsets
                            (newOffsetX, g5)
                              | flag == WeirdStars = randomR (-50, 50) g4
                              | otherwise = (0.0, g4)
                            (newOffsetY, g6)
                              | flag == WeirdStars = randomR (-50, 50) g5
                              | otherwise = (0.0, g5)
                            (newOffsetZ, _)
                              | flag == WeirdStars = randomR (-12.5, 12.5) g6
                              | otherwise = (0.0, g6)
                            -- Constants
                            maxPos = 10000.0 :: GLfloat
                                   

initfn :: IORef StarFlag
          -> IORef [StarRec]
          -> IO ()
initfn flag starList = do
  randGen <- newStdGen
  localFlag <- get flag
  starList $= newStarList randGen localFlag offset maxStars
  where
    offset = 100
    maxStars = 400