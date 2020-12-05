module Main where

-- data Position = Position Double Double
-- distance :: Position -> Position -> Double
-- distance p1 p2 = 
--     case p1 of 
--         Position x1 y1 ->
--             case p2 of
--                 Position x2 y2 ->
--                     sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
-- p1 = Position 0 1
-- p2 = Position 1 0

-- data Position = Double :+ Double
-- distance :: Position -> Position -> Double
-- distance p1 p2 = 
--     case p1 of 
--         x1 :+ y1 ->
--             case p2 of
--                 x2 :+ y2 ->
--                     sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
-- p1 = 0 :+ 1
-- p2 = 1 :+ 0

-- data Position = Position Double Double
-- distance :: Position -> Position -> Double
-- distance (Position x1 y1)(Position x2 y2) = 
--     sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
-- p1 = Position 0 1
-- p2 = Position 1 0

-- data Position = Position Double Double
-- distance :: Position -> Position -> Double
-- distance p1 p2 = 
--     let Position x1 y1 = p1
--         Position x2 y2 = p2
--     in sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
-- p1 = Position 0 1
-- p2 = Position 1 0

-- data Position = Cartesian Double Double | Polar Double Double
-- distance :: Position -> Position -> Double
-- distance (Cartesian x1 y1)(Cartesian x2 y2) = 
--     sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
-- distance (Cartesian x1 y1)(Polar r2 theta2) = 
--     let x2 = r2 * cos theta2
--         y2 = r2 * sin theta2
--     in sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
-- distance (Polar r1 theta1)(Cartesian x2 y2) = 
--     let x1 = r1 * cos theta1
--         y1 = r1 * sin theta1
--     in sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
-- distance (Polar r1 theta1)(Polar r2 theta2) = 
--     let x1 = r1 * cos theta1
--         y1 = r1 * sin theta1
--         x2 = r2 * cos theta2
--         y2 = r2 * sin theta2
--     in sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
-- p1 = Polar 1 0
-- p2 = Polar 1 (pi / 2)

data Position = Position { getX :: Double, getY :: Double }
pos = Position 1 2

-- l = 1 : 2 : []
l = [1, 2, 3, 4]
take2nd (x1:x2:rest) = x1 : x2 : []

takeNth :: [a] -> Int -> Maybe a
takeNth [] _ = Nothing
takeNth (x:xs) 0 = Just x
takeNth (x:xs) n = takeNth xs (n-1)
l2 = [1, 2, 3]

getLength :: [a] -> Int
getLength [] = 0
getLength (x:xs) = 1 + getLength xs
l3 = [1, 2, 3, 4]

takeN :: Int -> [a] -> [a]
takeN 0 _ = []
takeN _ [] = []
takeN n (x:xs) = x : takeN (n-1) xs

dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN _ [] = []
dropN n (x:xs) = dropN (n-1) xs

l4 = [1, 2, 3]

repeatN :: Int -> a -> [a]
repeatN 0 _ = []
repeatN n a = a : repeatN (n-1) a

sliceHelper :: Int -> (Int, Int) -> [a] -> [a]
sliceHelper _ (_, _) [] = []
sliceHelper i (a, b) (x:xs) = 
    if i < a || i > b then sliceHelper (i+1) (a, b) xs
                      else x : sliceHelper (i+1) (a, b) xs
slice :: (Int, Int) -> [a] -> [a]
slice (a, b) xs = sliceHelper 0 (a, b) xs

l5 = [1, 2, 3, 4, 5]
sl1 = slice (0, 1) l5
sl2 = slice (2, 3) l5
sl3 = slice (3, 10) l5
sl4 = slice (-1, 10) l5

enumerateFrom :: [Int] -> [a] -> [(Int, a)]
enumerateFrom _ [] = []
enumerateFrom [] _ = []
enumerateFrom (i:is) (x:xs) = (i, x) : enumerateFrom is xs
enumerate :: [a] -> [(Int, a)]
enumerate xs = enumerateFrom [0..] xs

sliceFromEnum :: (Int, Int) -> [(Int, a)] -> [a]
sliceFromEnum (a, b) [] = []
sliceFromEnum (a, b) ((i,x):xs) = 
    if i < a || i > b then sliceFromEnum (a, b) xs
                      else x : sliceFromEnum (a, b) xs
l6 = [1, 2, 3, 4, 5]
sl5 = sliceFromEnum (0, 1) (enumerate l6)
sl6 = sliceFromEnum (2, 3) (enumerate l6)
sl7 = sliceFromEnum (3, 10) (enumerate l6)
sl8 = sliceFromEnum (-1, 10) (enumerate l6)

zipF :: (a -> b -> c) -> [a] -> [b] -> [c]
zipF _ _ [] = []
zipF _ [] _ = []
zipF f (x:xs) (y:ys) = f x y : zipF f xs ys

zipFPadding :: a -> b -> (a -> b -> c) -> [a] -> [b] -> [c]
zipFPadding dft_a dft_b _ [] [] = []
zipFPadding dft_a dft_b f (x:xs) [] = f x dft_b : zipFPadding dft_a dft_b f xs []
zipFPadding dft_a dft_b f [] (y:ys) = f dft_a y : zipFPadding dft_a dft_b f [] ys
zipFPadding dft_a dft_b f (x:xs) (y:ys) = f x y : zipFPadding dft_a dft_b f xs ys

l7 = [1, 2, 3]
l8 = [4, 5, 6, 7, 8]
sum1 = zipF (+) l7 l8
sum2 = zipFPadding 0 0 (+) l7 l8

{------------------------ main function -------------------------}
main :: IO()
-- main = print (distance p1 p2)
-- main = print (getX pos)
-- main = print (takeNth l2 0, takeNth l2 1, takeNth l2 2, takeNth l2 3)
-- main = print (getLength l3)

-- main = print(takeN 2 l4, takeN 10 l4, dropN 2 l4, dropN 10 l4)

-- main = print (repeatN 3 1)

-- main = print (sl1, sl2, sl3, sl4)
-- main = print (sl5, sl6, sl7, sl8)

-- main = print (sum1, sum2)