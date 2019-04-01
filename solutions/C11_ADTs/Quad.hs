module C11_ADTs.Quad where

data Quad = 
    One'
  | Two'
  | Three'
  | Four'
  deriving (Eq, Show)

-- 1, how many different forms can this take? 
-- Answer -> 8, 4 + 4
eQuad :: Either Quad Quad 
eQuad = Right One'

eQuad' :: Either Quad Quad 
eQuad' = Right Two'

-- ...

eQuad'' :: Either Quad Quad 
eQuad'' = Right Four'

eQuadL :: Either Quad Quad
eQuadL = Left One' 

-- ...

eQuadL' :: Either Quad Quad
eQuadL' = Left Four' 

-- 2, how many...? 
-- Answer -> 16, 4 * 4
prodQuad :: (Quad, Quad)
prodQuad = (One', One')

-- ... 

prodQuad' :: (Quad, Quad)
prodQuad' = (One', Four') 

prodQuad'' :: (Quad, Quad)
prodQuad'' = (Two', One')

-- ... 

prodQuad''' :: (Quad, Quad)
prodQuad''' = (Four', Four')

-- 3
-- Answer 256, 4 ^ 4
funcQuad :: Quad -> Quad 
funcQuad One' = One' 
funcOne' Two' = One' 
funcOne' Three' = One' 
funcOne' Four' = One'

funcQuad' :: Quad -> Quad
funcQuad' One' = One'
funcQuad' Two' = One'
funcQuad' Three' = One'
funcQuad' Four' = Four'

-- ... 

funcQuad'' :: Quad -> Quad
funcQuad'' One' = Four'
funcQuad'' Two' = Three'
funcQuad'' Three' = One' 
funcQuad'' Four' = Two'

-- ...

-- Answer -> 8, 2 ^ 3
prodTBool :: (Bool, Bool, Bool)
prodTBool = (False, False, False)

prodTBool2 = (False, False, True)

prodTBool3 = (False, True, False)

prodTBool4 = (False, True, True)

prodTBool5 = (True, True, True)

prodTBool6 = (True, False, False)

prodTBool7 = (True, True, False)

prodTBool8 = (True, False, True)

-- 5, 
fTwo' :: Bool -> Quad -> Quad
fTwo' False One' = One' 
fTwo' False Two' = One' 
fTwo' False Three' = One' 
fTwo' False Four' = One' 
fTwo' True One' = One' 
fTwo' True Two' = One' 
fTwo' True Three' = One' 
fTwo' True Four' = One' 
-- ...
