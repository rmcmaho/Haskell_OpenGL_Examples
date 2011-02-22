module LightRec (

LightStruct(..),
LightList,
nullLightList,

) where

import Graphics.Rendering.OpenGL
import Data.IORef

-- | Object representing a light source.
-- Contains information regarding 'Light' and 'Face'.
data LightStruct = LightStruct { amb :: Color4 GLfloat -- ^ 'ambient'
                               ,diff :: Color4 GLfloat -- ^ 'diffuse'
                               ,spec :: Color4 GLfloat -- ^ 'specular'
                               ,pos :: Vertex4 GLfloat -- ^ 'position'
                               ,spotDir :: Normal3 GLfloat -- ^ 'spotDirection'
                               ,spotExp :: GLfloat -- ^ 'spotExponent'
                               ,cutoff :: GLfloat -- ^ 'spotCutoff'
                               ,atten :: (GLfloat, GLfloat, GLfloat) -- ^ 'attenuation'
                               ,trans :: Vector3 GLfloat -- ^ 'translate'
                               ,rot :: (GLfloat, GLfloat, GLfloat) -- ^ 'rotate'
                               ,swing :: (GLfloat, GLfloat, GLfloat) -- ^ Amount to swing around each axis
                               ,arc :: (GLfloat, GLfloat, GLfloat) -- ^ Current angle around each axis
                               ,arcIncr :: (GLfloat, GLfloat, GLfloat) -- ^ Rate at which to rotate around each axis
                               ,lightNum :: GLsizei -- ^ 'light'
                               }


type LightList = IORef [LightStruct]

                 
nullLightList :: IO LightList
nullLightList = newIORef []
                
