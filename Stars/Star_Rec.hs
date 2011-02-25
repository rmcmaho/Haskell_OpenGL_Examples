module Star_Rec where

data StarRec = StarRec { starType :: StarType,
                         x :: (GLfloat, GLfloat),
                         y :: (GLfloat, GLfloat),
                         z :: (GLfloat, GLfloat),
                         offsetX :: GLfloat,
                         offsetY :: GLfloat,
                         offsetR :: GLfloat,
                         rotation :: GLfloat}

data StarType =  Streak, Circle