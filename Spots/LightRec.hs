module LightRec (

LightStruct(..),
LightList,
nullLightList,

) where

import Graphics.Rendering.OpenGL
import Data.IORef

data LightStruct = LightStruct { amb :: Color4 GLfloat
													,diff :: Color4 GLfloat
													,spec :: Color4 GLfloat
													,pos :: Vertex4 GLfloat
													,spotDir :: Normal3 GLfloat
													,spotExp :: GLfloat
													,cutoff :: GLfloat
													,atten :: (GLfloat, GLfloat, GLfloat)
													,trans :: Vector3 GLfloat
													,rot :: (GLfloat, GLfloat, GLfloat)
													,swing :: (GLfloat, GLfloat, GLfloat)
													,arc :: (GLfloat, GLfloat, GLfloat)
													,arcIncr :: (GLfloat, GLfloat, GLfloat)
													,lightNum :: GLsizei
													}

type LightList = IORef [LightStruct]

nullLightList :: IO LightList
nullLightList = newIORef []
