{-# OPTIONS_HADDOCK ignore-exports #-}
module Spots_Display (

display,
idle,
initfn,
spotLights

) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Applicative

import LightRec


-- useful function
two_pi = 2*pi

-- | List of spotlights
spotLights :: [LightStruct]
spotLights = [LightStruct {amb=Color4 0.2 0.0 0.0 1.0, diff=Color4 0.8 0.0 0.0 1.0,
                           spec=Color4 0.4 0.0 0.0 1.0, pos=Vertex4 0.0 0.0 0.0 1.0,
                           spotDir=Normal3 0.0 (-1.0) 0.0, spotExp=20.0,
                           cutoff=60.0, atten=(1.0, 0.0, 0.0::GLfloat),
                           trans=Vector3 0.0 1.25 0.0, rot=(0.0, 0.0, 0.0),
                           swing=(20.0, 0.0, 40.0), arc=(0.0, 0.0, 0.0),
                           arcIncr=(two_pi / 70.0, 0.0, two_pi / 140.0),
                           lightNum=0},
              LightStruct {amb=Color4 0.0 0.2 0.0 1.0, diff=Color4 0.0 0.8 0.0 1.0,
                           spec=Color4 0.0 0.4 0.0 1.0, pos=Vertex4 0.0 0.0 0.0 1.0,
                           spotDir=Normal3 0.0 (-1.0) 0.0, spotExp=20.0,
                           cutoff=60.0, atten=(1.0, 0.0, 0.0::GLfloat),
                           trans=Vector3 0.0 1.25 0.0, rot=(0.0, 0.0, 0.0),
                           swing=(20.0, 0.0, 40.0), arc=(0.0, 0.0, 0.0),
                           arcIncr=(two_pi / 120.0, 0.0, two_pi / 60.0),
                           lightNum=1},
              LightStruct {amb=Color4 0.0 0.0 0.2 1.0, diff=Color4 0.0 0.0 0.8 1.0,
                           spec=Color4 0.0 0.0 0.4 1.0, pos=Vertex4 0.0 0.0 0.0 1.0,
                           spotDir=Normal3 0.0 (-1.0) 0.0, spotExp=20.0,
                           cutoff=60.0, atten=(1.0, 0.0, 0.0::GLfloat),
                           trans=Vector3 0.0 1.25 0.0, rot=(0.0, 0.0, 0.0),
                           swing=(20.0, 0.0, 40.0), arc=(0.0, 0.0, 0.0),
                           arcIncr=(two_pi / 50.0, 0.0, two_pi / 100.0),
                           lightNum=2}]
             
-- |Display callback.
display :: (HasGetter g)
           => g GLfloat -- ^ Current rotation angle
           -> LightList -- ^ List of spotlights
           -> IO ()
display spin lightList = do
	clear [ColorBuffer,DepthBuffer]
	preservingMatrix $ do
        preservingMatrix $ do
		angle <- get spin
		myList <- get lightList
		rotate angle $ Vector3 0.0 1.0 (0.0::GLfloat)
		
		lightList $= aimLights myList
		myList <- get lightList
		setLights myList
		drawLights myList
		
		preservingMatrix $ do
			rotate (-90.0) $ Vector3 1.0 0.0 (0::GLfloat)
			scale 1.9 1.9 (1.0::GLfloat)
			translate $ Vector3 (-0.5) (-0.5) (0.0::GLfloat)
			drawPlane 16 16
	swapBuffers
	flush

-- | Initializes each light source in the list
initLights :: [LightStruct] -> IO ()
initLights = mapM_ initLight

-- | Initializes a light source
initLight :: LightStruct -> IO()
initLight lt = do
	light (Light $ lightNum lt) $= Enabled
	ambient (Light $ lightNum lt) $= amb lt
	diffuse (Light $ lightNum lt) $= diff lt
	specular (Light $ lightNum lt) $= spec lt
	spotExponent (Light $ lightNum lt) $= spotExp lt
	spotCutoff (Light $ lightNum lt) $= cutoff lt
	attenuation (Light $ lightNum lt) $= atten lt

-- | Aims each light source in the list and returns a new list with aimed lights.
aimLights :: [LightStruct] -- ^ Original list of lights
             -> [LightStruct] -- ^ New, aimed list of lights
aimLights = map rotateLight

-- | Aims a light source and returns the new aimed light.
rotateLight :: LightStruct -- ^ Original light
               -> LightStruct -- ^ New, aimed light
rotateLight lt = LightStruct {amb=amb lt, diff=diff lt,
	spec=spec lt, pos=pos lt,
	spotDir=spotDir lt, spotExp=spotExp lt,
	cutoff=cutoff lt, atten=atten lt,
	trans=trans lt, 
	rot=newRot,
	swing=swing lt,
        arc= newArc,
	arcIncr=arcIncr lt,
	lightNum=lightNum lt}
	where
          -- Set new swing angle
          (swingX, swingY, swingZ) = swing lt
          newRot = (swingX * sin arcX, swingY * sin arcY, swingZ * sin arcZ)
          -- Set new arc angle
          (arcX, arcY, arcZ) = modArcTuple . arc $ lt
          (arcIncX, arcIncY, arcIncZ) = arcIncr lt
          newArc = (arcX+arcIncX / speed_factor, arcY+arcIncY/ speed_factor, arcZ+arcIncZ/ speed_factor)
          -- Utility stuff
          speed_factor = 10
          modArcTuple (arcX, arcY, arcZ) = (modArc arcX, modArc arcY, modArc arcZ)
            where modArc arc
                    | arc > two_pi = arc - two_pi
                    | otherwise = arc
		
-- | Sets the lights to their current values
setLights :: [LightStruct] -> IO()
setLights = mapM_ setLight

-- | Sets a light to its current value
setLight :: LightStruct -> IO()
setLight lt = preservingMatrix $ do
		translate $ trans lt
		rotate rotX $ Vector3 1.0 0.0 (0.0::GLfloat)
		rotate rotY $ Vector3 0.0 1.0 (0.0::GLfloat)
		rotate rotZ $ Vector3 0.0 0.0 (1.0::GLfloat)
		position (Light $ lightNum lt) $= pos lt
		spotDirection (Light $ lightNum lt) $= spotDir lt
	where
		(rotX, rotY, rotZ) = rot lt

-- | Draws the lights
drawLights :: [LightStruct] -> IO ()
drawLights = mapM_ drawLight

-- | Draws a light
drawLight :: LightStruct -> IO()
drawLight lt = do
	lighting $= Disabled	
	color $ diff lt
	preservingMatrix $ do
		translate $ trans lt
		rotate rotX $ Vector3 1.0 0.0 (0.0::GLfloat)
		rotate rotY $ Vector3 0.0 1.0 (0.0::GLfloat)
		rotate rotZ $ Vector3 0.0 0.0 (1.0::GLfloat)
		renderPrimitive Lines $ do
			vertex $ normalToVertex (spotDir lt)
			vertex $ pos lt
	lighting $= Enabled
	where
          (rotX, rotY, rotZ) = rot lt
          normalToVertex (Normal3 x y z) = Vertex3 x y z	

-- | Draws a plane (dance floor) with the specified width and height.
drawPlane :: Float -- ^ Width of the plane
             -> Float -- ^ Height of the plane
             -> IO ()
drawPlane w h = do
	normal $ Normal3 0.0 0.0 (1.0::GLfloat)
	mapM_ (\(x,y) -> preservingMatrix $ renderSection (x,y)) $ myPoints w h
        where
          -- | Points of a dance floor.		
          myPoints w h = (,) <$> [0..w] <*> [0..(h-1)]

-- | Draw a section of the dance floor
renderSection :: (GLfloat,GLfloat) -> IO ()	
renderSection (y,x) = 
  renderPrimitive TriangleStrip $ do
    vertex $ Vertex2 (delta*x) (delta*(y+1.0)::GLfloat)
    vertex $ Vertex2 (delta*x) (delta*y::GLfloat)
    vertex $ Vertex2 (delta*(x-1.0)) (delta*y::GLfloat)
    where delta = 1.0/16.0

-- | Idle callback. Increments the simulation.
-- Runs whenever OpenGL has nothing else to do.
idle :: (HasGetter g, Fractional a, Ord a, HasSetter g) => g a -> IO ()
idle spin = do
	angle <- get spin
	spin $= modAngle angle + 0.1
	postRedisplay Nothing
        where modAngle a
                | a > 360.0 = -360.0
                | a < (-360.0) = 360.0
                | otherwise = a	

-- | Initializes various things for the program
initfn :: IO ()
initfn = do
	depthFunc $= Just Less

	matrixMode $= Projection
	frustum (-1) 1 (-1) 1 2 (6::GLdouble) --glFrustum
	matrixMode $= Modelview 0
	
	translate $ Vector3 0.0 0.0 (-3.0::GLfloat)
	rotate 45.0 $ Vector3 1.0 0.0 (0.0::GLfloat)
	
	lighting $= Enabled
	normalize $= Enabled

	lightModelAmbient $= modelAmb
	lightModelLocalViewer  $= Enabled
	lightModelTwoSide  $= Disabled
	
	materialAmbient Front $= matAmb
	materialDiffuse Front $= matDiff
	materialSpecular Front $= matSpec
	materialEmission Front $= matEmission
	materialShininess Front $= 10.0
	
	initLights spotLights
        where
          modelAmb = Color4 0.2 0.2 0.2 (1.0::GLfloat)
          matAmb = Color4 0.2 0.2 0.2 (1.0::GLfloat)
          matDiff = Color4 0.8 0.8 0.8 (1.0::GLfloat)
          matSpec = Color4 0.4 0.4 0.4 (1.0::GLfloat)
          matEmission = Color4 0.0 0.0 0.0 (1.0::GLfloat)
