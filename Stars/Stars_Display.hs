module Stars_Display (
  reshape,
  keyboardMouse,
  display,
  visible,
  initfn
  ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

reshape :: Size -> IO ()
reshape _ = return ()

keyboardMouse :: Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse _ _ _ _ = return ()

display :: IO ()
display = return ()

visible :: Visibility -> IO ()
visible _ = return ()

initfn :: IO ()
initfn = return ()