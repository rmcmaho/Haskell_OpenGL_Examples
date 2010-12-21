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
speed_factor = 10

-- list of spotlights
spotLights :: [LightStruct]
spotLights = [LightStruct {amb=Color4 0.2 0.0 0.0 1.0, diff=Color4 0.8 0.0 0.0 1.0,
												spec=Color4 0.4 0.0 0.0 1.0, pos=Vertex4 0.0 0.0 0.0 1.0,
												spotDir=Normal3 0.0 (-1.0) 0.0, spotExp=20.0,
												cutoff=60.0, atten=(1.0, 0.0, (0.0::GLfloat)),
												trans=Vector3 0.0 1.25 0.0, rot=(0.0, 0.0, 0.0),
												swing=(20.0, 0.0, 40.0), arc=(0.0, 0.0, 0.0),
												arcIncr=(two_pi / 70.0, 0.0, two_pi / 140.0),
												lightNum=0},
							LightStruct {amb=Color4 0.0 0.2 0.0 1.0, diff=Color4 0.0 0.8 0.0 1.0,
												spec=Color4 0.0 0.4 0.0 1.0, pos=Vertex4 0.0 0.0 0.0 1.0,
												spotDir=Normal3 0.0 (-1.0) 0.0, spotExp=20.0,
												cutoff=60.0, atten=(1.0, 0.0, (0.0::GLfloat)),
												trans=Vector3 0.0 1.25 0.0, rot=(0.0, 0.0, 0.0),
												swing=(20.0, 0.0, 40.0), arc=(0.0, 0.0, 0.0),
												arcIncr=(two_pi / 120.0, 0.0, two_pi / 60.0),
												lightNum=1},
							LightStruct {amb=Color4 0.0 0.0 0.2 1.0, diff=Color4 0.0 0.0 0.8 1.0,
												spec=Color4 0.0 0.0 0.4 1.0, pos=Vertex4 0.0 0.0 0.0 1.0,
												spotDir=Normal3 0.0 (-1.0) 0.0, spotExp=20.0,
												cutoff=60.0, atten=(1.0, 0.0, (0.0::GLfloat)),
												trans=Vector3 0.0 1.25 0.0, rot=(0.0, 0.0, 0.0),
												swing=(20.0, 0.0, 40.0), arc=(0.0, 0.0, 0.0),
												arcIncr=(two_pi / 50.0, 0.0, two_pi / 100.0),
												lightNum=2}]

-- display callback function
display spin lightList = do
	clear [ColorBuffer,DepthBuffer]
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

-- init lights
initLights = do
	mapM_ initLight spotLights

initLight :: LightStruct -> IO()
initLight lt = do
	light (Light $ lightNum lt) $= Enabled
	ambient (Light $ lightNum lt) $= amb lt
	diffuse (Light $ lightNum lt) $= diff lt
	specular (Light $ lightNum lt) $= spec lt
	spotExponent (Light $ lightNum lt) $= spotExp lt
	spotCutoff (Light $ lightNum lt) $= cutoff lt
	attenuation (Light $ lightNum lt) $= atten lt

-- aim lights
aimLights :: [LightStruct] -> [LightStruct]
aimLights lightList = map (rotateLight) lightList

rotateLight lt = LightStruct {amb=amb lt, diff=diff lt,
	spec=spec lt, pos=pos lt,
	spotDir=spotDir lt, spotExp=spotExp lt,
	cutoff=cutoff lt, atten=atten lt,
	trans=trans lt, 
	rot=(swingX * (sin arcX), swingY * (sin arcY), swingZ * (sin arcZ)),
	swing=swing lt, arc=(arcX+arcIncX / speed_factor, arcY+arcIncY/ speed_factor, arcZ+arcIncZ/ speed_factor),
	arcIncr=arcIncr lt,
	lightNum=lightNum lt}
	where
		(swingX, swingY, swingZ) = swing lt
		(arcX, arcY, arcZ) = modArcTuple . arc $ lt
		(arcIncX, arcIncY, arcIncZ) = arcIncr lt

modArcTuple (arcX, arcY, arcZ) = (modArc arcX, modArc arcY, modArc arcZ)

modArc arc
	| arc > two_pi = arc - two_pi
  | otherwise = arc
		
-- set lights
setLights myList= do
	mapM_ setLight myList

setLight :: LightStruct -> IO()
setLight lt = do
	preservingMatrix $ do
		translate $ trans lt
		--print rotX
		rotate rotX $ Vector3 1.0 0.0 (0.0::GLfloat)
		rotate rotY $ Vector3 0.0 1.0 (0.0::GLfloat)
		rotate rotZ $ Vector3 0.0 0.0 (1.0::GLfloat)
		position (Light $ lightNum lt) $= pos lt
		spotDirection (Light $ lightNum lt) $= spotDir lt
	where
		(rotX, rotY, rotZ) = rot lt

-- draw lights
drawLights myList= do
	mapM_ drawLight myList

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

normalToVertex :: Normal3 GLfloat -> Vertex3 GLfloat
normalToVertex (Normal3 x y z) = Vertex3 x y z	
	
drawPlane w h = do
	normal $ Normal3 0.0 0.0 (1.0::GLfloat)
	mapM_ (\(x,y) -> preservingMatrix $ do
		renderSection (x,y)
		) $ myPoints w h


		
myPoints :: Float -> Float -> [(GLfloat,GLfloat)]
myPoints w h = (,) <$> [0..w] <*> [0..(h-1)]


renderSection :: (GLfloat,GLfloat) -> IO ()	
renderSection (y,x) = do
	renderPrimitive TriangleStrip $ do
		--print $ Vertex2 (d*x) (d*(y+1))
		--print $ Vertex2 (d*x) (d*y)
		vertex $ Vertex2 (d*x) ((d*(y+1.0))::GLfloat)
		vertex $ Vertex2 (d*x) ((d*y)::GLfloat)
		vertex $ Vertex2 (d*(x-1.0)) ((d*y)::GLfloat)

renderSection' (y,x) = do
	renderPrimitive TriangleStrip $ do
		vertex $ Vertex2 (1.0::GLfloat) (-1.0)
		vertex $ Vertex2 (0.0) (1.0::GLfloat)
		vertex $ Vertex2 (-1.0) ((-1.0)::GLfloat)

d = 1.0/16.0

idle spin = do
	angle <- get spin
	spin $= (modAngle angle) + 0.1
	postRedisplay Nothing

modAngle a
    | a > 360.0 = (-360.0)
    | a < (-360.0) = 360.0
    | otherwise = a	
	
initfn = do
	depthFunc $= Just Less

	matrixMode $= Projection
	frustum (-1) 1 (-1) 1 2 (6::GLdouble) --glFrustum
	matrixMode $= Modelview 0
	
	translate $ Vector3 0.0 0.0 ((-3.0)::GLfloat)
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
	
	initLights
	
modelAmb :: Color4 GLfloat
modelAmb = Color4 0.2 0.2 0.2 (1.0::GLfloat)

matAmb = Color4 0.2 0.2 0.2 (1.0::GLfloat)
matDiff = Color4 0.8 0.8 0.8 (1.0::GLfloat)
matSpec = Color4 0.4 0.4 0.4 (1.0::GLfloat)
matEmission = Color4 0.0 0.0 0.0 (1.0::GLfloat)