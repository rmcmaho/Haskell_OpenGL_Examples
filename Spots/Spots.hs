
module Main (

main

) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data.IORef

import Spots_Display

main = do
	(progname,_) <- getArgsAndInitialize
	initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
	createWindow "GLUT spotlight swing"
	
	spin <- newIORef (0.0::GLfloat)
	lightList <- newIORef spotLights
	
	displayCallback $= display spin lightList
	idleCallback $= Just (idle spin)
	
	initfn
	
	mainLoop