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

import qualified Star_Rec as Star

maxWarp :: GLfloat
maxWarp = 10.0
speed :: GLfloat
speed = 0.1
maxPos :: GLfloat
maxPos = 10000.0
maxAngles :: GLfloat
maxAngles = 6000

reshape :: Size -> IO ()
reshape (Size width height) = do
  viewport $= (Position 0 0, Size width height)
  matrixMode $= Projection
  loadIdentity
  ortho2D (-0.5) (fromIntegral width+0.5) (-0.5) (fromIntegral height+0.5)
  matrixMode $= Modelview 0

keyboardMouse :: Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse _ _ _ _ = return ()

display :: IORef [Star.StarRec]
           -> IO ()
display starListRef = do
  starList <- get starListRef
  (_, winDimensions) <- get viewport
  clear [ColorBuffer]
  showStars winDimensions starList
  swapBuffers
  

showStars :: Size
             -> [Star.StarRec]
             -> IO ()
showStars winDimensions = mapM_ (showStar winDimensions)

showStar :: Size
            -> Star.StarRec
            -> IO ()
showStar (Size windW windH) star  
  | x0 < 0.0 || x0 > numW || y0 < 0.0 || y0 > numH = return () -- skip this star if it is off the screen
  | Star.starType star == Star.Streak = drawStreak
  | Star.starType star == Star.Circle = drawCircle
  | otherwise = return ()
  where
    numW = fromIntegral windW
    numH = fromIntegral windH
    -- 'br' means 'before rotation'
    x0_br = (fst . Star.x $ star) * numW / (fst . Star.z $ star)
    y0_br = (fst . Star.y $ star) * numH / (fst . Star.z $ star)
    (x0, y0) = (\(xTmp, yTmp) -> (xTmp+(numW/2.0), yTmp+(numH/2.0))) . rotatePoint x0_br y0_br $ Star.rotation star
    x1_br = (snd . Star.x $ star) * numW / (snd . Star.z $ star)
    y1_br = (snd . Star.y $ star) * numH / (snd . Star.z $ star)
    (x1, y1) = (\(xTmp, yTmp) -> (xTmp+(numW/2.0), yTmp+(numH/2.0))) . rotatePoint x1_br y1_br $ Star.rotation star
    drawStreak
      | abs (x0 - x1) < 1.0 && abs (y0 - y1) < 1.0 = do
        color $ Color3 1.0 ((maxWarp - speed) / maxWarp) ((maxWarp - speed) / maxWarp)
        renderPrimitive Points $ do
          vertex $ Vertex2 x0 y0
      | otherwise = do
        color $ Color3 1.0 ((maxWarp - speed) / maxWarp) ((maxWarp - speed) / maxWarp)
        renderPrimitive Lines $ do
          vertex $ Vertex2 x0 y0
          vertex $ Vertex2 x1 y1
    drawCircle = do
      color $ Color3 1.0 0.0 (0.0::GLfloat)
      renderPrimitive Polygon $ mapM_ generateVertex [0..7]
        where
          width = maxPos / 10.0 / (fst . Star.z $ star) + 1.0
          generateVertex i = vertex $ Vertex2 (newX i) (newY i)
            where
              newX i = x0 + width * cos (i * maxAngles / 8.0)
              newY i = y0 + width * sin (i * maxAngles / 8.0)
        

rotatePoint :: GLfloat
               -> GLfloat
               -> GLfloat
               -> (GLfloat, GLfloat)
rotatePoint xIn yIn rotationAngle = (xOut, yOut)
  where
    xOut = xIn * cos rotationAngle - yIn * sin rotationAngle
    yOut = yIn * cos rotationAngle + xIn * sin rotationAngle

visible :: IORef [Star.StarRec] -> Visibility -> IO ()
visible starListRef Visible = idleCallback $= Just (idle starListRef)
visible _ _ = return ()

idle :: IORef [Star.StarRec] -> IO ()
idle starListRef = do
  starList <- get starListRef
  randomGen <- newStdGen
  (_, winDimensions) <- get viewport
  starListRef $= updateStars winDimensions randomGen (moveStars starList)
  postRedisplay Nothing

moveStars :: [Star.StarRec] -> [Star.StarRec]
moveStars starList = map moveStar starList

moveStar :: Star.StarRec -> Star.StarRec
moveStar star = Star.StarRec {Star.starType = Star.starType star, Star.x = (newX0, newX1), 
                              Star.y = (newY0, newY1), Star.z = (newZ0, newZ1),
                              Star.offsetX = Star.offsetX star, Star.offsetY = Star.offsetY star, 
                              Star.offsetR = Star.offsetR star, Star.rotation = newRotation}
                where
                  newX0 = (fst . Star.x $ star) + Star.offsetX star
                  newY0 = (fst . Star.y $ star) + Star.offsetY star
                  offset = speed * 60.0
                  newZ0 = (fst . Star.z $ star) - offset
                  newX1 = (fst . Star.x $ star)
                  newY1 = (fst . Star.y $ star)
                  newZ1 = (fst . Star.z $ star)
                  tempRot = Star.rotation star + Star.offsetR star 
                  newRotation
                    | tempRot > maxAngles = 0.0
                    | otherwise = tempRot

updateStars :: Size -> StdGen -> [Star.StarRec] -> [Star.StarRec]
updateStars winDimensions gen (star:[]) = updateStar winDimensions gen1 star:[]
  where
    (gen1, gen2) = split gen
updateStars winDimensions gen (star:starList) = updateStar winDimensions gen1 star:updateStars winDimensions gen2 starList
  where
    (gen1, gen2) = split gen

updateStar winDimensions gen star 
  | z0 > speed || (z0 > 0.0 && speed < maxWarp) = if starPoint winDimensions star then newStar gen Star.NormalStars (floor maxPos) else star
  | otherwise = newStar gen Star.NormalStars (floor maxPos)
                where z0 = fst . Star.z $ star

starPoint :: Size -> Star.StarRec -> Bool
starPoint (Size windW windH) star = not (x0 >= 0.0 && x0 < numW && y0 >= 0.0 && y0 < numH)
  where
    numW = fromIntegral windW
    numH = fromIntegral windH
    x0_br = (fst . Star.x $ star) * numW / (fst . Star.z $ star)
    y0_br = (fst . Star.y $ star) * numH / (fst . Star.z $ star)
    (x0, y0) = (\(xTmp, yTmp) -> (xTmp+(numW/2.0), yTmp+(numH/2.0))) . rotatePoint x0_br y0_br $ Star.rotation star


-- | Creates a new random star
newStarList :: StdGen -- ^ Random number generator
               -> Star.StarFlag -- ^ Flag for stars
               -> GLint -- ^ Z-axis Offset
               -> GLint -- ^ Number of stars to make
               -> [Star.StarRec] -- ^ List of new stars
newStarList gen flag starDepth numStars
  | numStars > 0 = newStar gen1 flag starDepth:newStarList gen2 flag starDepth (numStars - 1)
  | otherwise = []
  where
    (gen1, gen2) = split gen
    
newStar :: StdGen
           -> Star.StarFlag
           -> GLint
           -> Star.StarRec
newStar gen flag starDepth = Star.StarRec {Star.starType = getType randomType, Star.x = (starX, starX),
                                   Star.y = (starY, starY), Star.z = (starZ, starZ),
                                   Star.offsetX = newOffsetX, Star.offsetY = newOffsetY,
                                   Star.offsetR = newOffsetR, Star.rotation = 0.0}
                          where
                            -- Generate type
                            (randomType, g1) = randomR (0,4) gen
                            getType :: Int -> Star.StarType
                            getType 0 = Star.Circle
                            getType _ = Star.Streak
                            -- Get position
                            (starX, g2) = randomR (negate (maxPos / 2.0), maxPos / 2.0) g1
                            (starY, g3) = randomR (negate (maxPos / 2), maxPos / 2) g2
                            (starZ, g4) = randomR (fromIntegral starDepth, maxPos + fromIntegral starDepth) g3
                            -- Get offsets
                            (newOffsetX, g5)
                              | flag == Star.WeirdStars = randomR offSetSeedX g4
                              | otherwise = (0.0, g4)
                            (newOffsetY, g6)
                              | flag == Star.WeirdStars = randomR offSetSeedY g5
                              | otherwise = (0.0, g5)
                            (newOffsetR, _)
                              | flag == Star.WeirdStars = randomR offSetSeedR g6
                              | otherwise = (0.0, g6)
                            -- Constants
                            maxPos = 10000.0 :: GLfloat
                            offSetSeedX = (-50,50)
                            offSetSeedY = (-50,50)
                            offSetSeedR = (-12.5, 12.5)
                                   

initfn :: IORef Star.StarFlag
          -> IORef [Star.StarRec]
          -> IO ()
initfn flag starList = do
  randGen <- newStdGen
  localFlag <- get flag
  starList $= newStarList randGen localFlag offset maxStars
  clearColor $= Color4 0.0 0.0 0.0 0.0
  dither $= Disabled
  where
    offset = 100
    maxStars = 400