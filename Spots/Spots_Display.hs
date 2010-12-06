module Spots_Display (

display,
idle,
initfn

) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Applicative

display = do
	clear [ColorBuffer];
	--loadIdentity;
	--translate $ Vector3 0.0 0.0 ((-3.0)::GLfloat)
	--rotate 45.0 $ Vector3 1.0 0.0 (0.0::GLfloat)
	
	preservingMatrix $ do
		scale 1.9 1.9 (1.0::GLfloat)
		translate $ Vector3 (-0.5) (-0.5) (0.0::GLfloat)
		rotate (-90.0) $ Vector3 1.0 0.0 (0::GLfloat)
		drawPlane

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
  --specular (Light 0) $= Color4 0.4 0.0 0.0 1.0
	
drawPlane = do
	normal $ Normal3 0.0 0.0 (1.0::GLfloat)
	mapM_ (\(x,y) -> preservingMatrix $ do
		--scale 1.9 1.9 (1.0::GLfloat)
		renderSection (x,y)
		) $ myPoints


		
myPoints :: [(GLfloat,GLfloat)]
myPoints = (,) <$> [0..16] <*> [0..15]


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

idle = do
	postRedisplay Nothing

initfn = do
	matrixMode $= Projection
	--frustum (-1) 1 (-1) 1 2 (6::GLdouble) --glFrustum
	matrixMode $= Modelview 0
	
	--translate $ Vector3 0.0 0.0 ((-3.0)::GLfloat)
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
	--light (Light 0) $= Enabled
	--ambient (Light 0) $= lightAmbient
	
	--depthFunc $= Just Less
	--position (Light 0) $= Vertex4 0.0 0.0 0.0 (1.0);
	--spotDirection (Light 0) $= (Normal3 0.0 (-1.0) 0.0);
	
	initLights
	
modelAmb :: Color4 GLfloat
modelAmb = Color4 0.2 0.2 0.2 (1.0::GLfloat)

matAmb = Color4 0.2 0.2 0.2 (1.0::GLfloat)
matDiff = Color4 0.8 0.8 0.8 (1.0::GLfloat)
matSpec = Color4 0.4 0.4 0.4 (1.0::GLfloat)
matEmission = Color4 0.0 0.0 0.0 (1.0::GLfloat)