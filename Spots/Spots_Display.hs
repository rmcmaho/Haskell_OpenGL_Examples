module Spots_Display (

display,
idle,
initfn

) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Applicative

import LightRec


-- useful function
two_pi = 2*pi

spotLights :: [LightStruct]
spotLights = [LightStruct {amb=Color4 0.2 0.0 0.0 1.0, diff=Color4 0.8 0.0 0.0 1.0,
												spec=Color4 0.4 0.0 0.0 1.0, pos=Vertex4 0.4 0.0 0.0 1.0,
												spotDir=Normal3 0.0 (-1.0) 0.0, spotExp=20.0,
												cutoff=60.0, atten=(1.0, 0.0, (0.0::GLfloat)),
												trans=Vertex3 0.0 1.25 0.0, rot=(0.0, 0.0, 0.0),
												swing=(20.0, 0.0, 40.0), arc=(0.0, 0.0, 0.0),
												arcIncr=(two_pi / 70.0, 0.0, two_pi / 140.0)}]

display spin = do
	clear [ColorBuffer,DepthBuffer]
	preservingMatrix $ do
		angle <- get spin
		rotate angle $ Vector3 0.0 1.0 (0.0::GLfloat)
		
		setLights
		drawLights
		
		preservingMatrix $ do
			rotate (-90.0) $ Vector3 1.0 0.0 (0::GLfloat)
			scale 1.9 1.9 (1.0::GLfloat)
			translate $ Vector3 (-0.5) (-0.5) (0.0::GLfloat)
			drawPlane 16 16
		
		

{-
	preservingMatrix $ do
		renderPrimitive Quads $ do
			vertex $ Vertex3 (-0.5) 0.5 ((-0.5)::GLfloat)
			vertex $ Vertex3 (-0.5) (-0.5) ((-0.5)::GLfloat)
			vertex $ Vertex3 0.5 (-0.5) ((-0.5)::GLfloat)
			vertex $ Vertex3 0.5 0.5 ((-0.5)::GLfloat);
-}
	swapBuffers
	flush

initLights = do
	light (Light 0) $= Enabled
	ambient (Light 0) $= Color4 0.2 0.0 0.0 1.0
	diffuse (Light 0) $= Color4 0.8 0.0 0.0 1.0
	specular (Light 0) $= Color4 0.4 0.0 0.0 1.0
	spotExponent (Light 0) $= 20.0
	spotCutoff (Light 0) $= 60.0
	attenuation (Light 0) $= (1.0, 0.0, (0.0::GLfloat))

setLights = do
	preservingMatrix $ do
		translate $ Vector3 0.0 1.25 (0.0::GLfloat)
		position (Light 0) $= Vertex4 0.0 0.0 0.0 1.0
		spotDirection (Light 0) $= Normal3 0.0 (-1.0) 0.0

drawLights = do
	lighting $= Disabled	
	color $ Color3 0.8 0.0 (0.0::GLfloat)	
	preservingMatrix $ do
		translate $ Vector3 0.0 1.25 (0.0::GLfloat)
		renderPrimitive Lines $ do
			vertex $ Vertex3 0.0 0.0 (0.0::GLfloat)
			vertex $ Vertex3 0.0 (-1.0) (0.0::GLfloat)
	lighting $= Enabled
	
	
drawPlane w h = do
	normal $ Normal3 0.0 0.0 (1.0::GLfloat)
	mapM_ (\(x,y) -> preservingMatrix $ do
		--scale 1.9 1.9 (1.0::GLfloat)
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
	spin $= (modAngle angle) + 0.5
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