module Tri_Objects where

import Graphics.Rendering.OpenGL
import Data.IORef

import Random

data TriObject = TriObject {v1 :: Vertex2 GLfloat,
                            v2 :: Vertex2 GLfloat,
                            v3 :: Vertex2 GLfloat,
                            color :: Color3 GLfloat
                            } deriving (Show)

data TriObjectList = IORef [TriObject]

randomObject :: StdGen -> TriObject
randomObject gen = TriObject (Vertex2 (x + v1_X - 25) (y + v1_Y - 25))
                   (Vertex2 (x + v2_X - 25) (y + v2_Y - 25))
                   (Vertex2 (x + v3_X - 25) (y + v3_Y - 25))
                   (Color3 ((c1 + 50) / 150.0) ((c2 + 50) / 150.0) ((c3 + 50) / 150.0) )
  where
    (initX, g1) = randomR (0,300) gen
    (initY, g2) = randomR (0,300) g1
    x = initX - 150
    y = initY - 150
    -- Get x verticies
    (v1_X, g3) = randomR (0,50) g2
    (v2_X, g4) = randomR (0,50) g3
    (v3_X, g5) = randomR (0,50) g4
    -- Get y verticies
    (v1_Y, g6) = randomR (0,50) g5
    (v2_Y, g7) = randomR (0,50) g6
    (v3_Y, g8) = randomR (0,50) g7
    -- Get color values
    c1:c2:c3:[] = take 3 $ randomRs (0,100) g8