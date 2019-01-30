module Chap9.ListComps where

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

-- 1
tups = [(x, y) | x <- mySqr, y <- myCube]

-- 2
tupsTwo = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- 3
numTups = length tups
