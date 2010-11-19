
module Main (

main

) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Spots_Display

main = do
	(progname,_) <- getArgsAndInitialize
	initialDisplayMode $= [DoubleBuffered, RGBMode]
	createWindow "GLUT spotlight swing"
	
	displayCallback $= (display)
	idleCallback $= Just (idle)
	
	initfn
	
	mainLoop