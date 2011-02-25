module Stars_Display (
  reshape,
  keyboardMouse,
  display,
  visible,
  initfn
  ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Star_Rec

reshape :: Size -> IO ()
reshape (Size width height) = do
  viewport $= (Position 0 0, Size width height)
  matrixMode $= Projection
  loadIdentity
  ortho2D (-0.5) (fromIntegral width+0.5) (-0.5) (fromIntegral height+0.5)
  matrixMode $= Modelview 0

keyboardMouse :: Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse _ _ _ _ = return ()

display :: IO ()
display = return ()

visible :: Visibility -> IO ()
visible _ = return ()

initfn :: IO ()
initfn = return ()