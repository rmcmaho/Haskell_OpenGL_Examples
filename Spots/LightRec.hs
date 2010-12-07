module LightRec (

LightStruct(..)

) where

import Graphics.Rendering.OpenGL

data LightStruct = LightStruct { amb :: Color4 GLfloat
													,diff :: Color4 GLfloat
													,spec :: Color4 GLfloat
													,pos :: Vertex4 GLfloat
													,spotDir :: Normal3 GLfloat
													,spotExp :: GLfloat
													,cutoff :: GLfloat
													,atten :: (GLfloat, GLfloat, GLfloat)
													,trans :: Vertex3 GLfloat
													,rot :: (GLfloat, GLfloat, GLfloat)
													,swing :: (GLfloat, GLfloat, GLfloat)
													,arc :: (GLfloat, GLfloat, GLfloat)
													,arcIncr :: (GLfloat, GLfloat, GLfloat)
													}