{-# LANGUAGE MultiWayIf #-}

module TestFn where

import Data.Function

num1 = 2 & (+2) & (2^)
num2 = 2 & (\x -> x + 2) & (\x -> 2 ^ x)

addTwo :: Int -> Int
addTwo x = x + 2
powTwo :: Int -> Int
powTwo x = 2 ^ x
num3 = 2 & addTwo & powTwo

makeSqr :: (Floating a) => (a, a) -> (a, a)
makeSqr (a, b) = (a * a, b * b)
makeSum :: (Floating a) => (a, a) -> a
makeSum (a, b) = (+) a b
makeRoot :: (Floating a) => a -> a
makeRoot a = sqrt a
makeNorm :: (Floating a) => (a, a) -> a
makeNorm (a, b) = makeRoot . makeSum . makeSqr $ (a, b)
num4 = makeNorm (3, 4)

-- takeEven :: [a] -> [a]
-- takeEven xs = takeEvenImpl $ zip [0..] xs
--     where 
--         takeEvenImpl [] = []
--         takeEvenImpl ((i,x):xs) = if i `rem` 2 == 0 then x : takeEvenImpl xs
--                                                     else takeEvenImpl xs
-- l1 = takeEven [1, 2, 3, 4, 5]

takeEven :: [a] -> [a]
takeEven xs = takeEvenImpl $ zip [0..] xs
    where 
        takeEvenImpl [] = []
        takeEvenImpl ((i,x):xs) = case i `rem` 2 of
                                        0 -> x : takeEvenImpl xs
                                        _ -> takeEvenImpl xs
l1 = takeEven [1, 2, 3, 4, 5]

-- inRange :: (Floating a, Ord a) => a -> String
-- inRange x
--     | y < 0 = "<0"
--     | y >= 0 && y < 10 = "0<= < 10"
--     | y >= 10 && y < 100 = "10<= < 100"
--     | otherwise = "large"
--     where y = x * 10

inRange :: (Floating a, Ord a) => a -> String
inRange x = 
    if  | y < 0 -> "<0"
        | y >= 0 && y < 10 -> "0<= < 10"
        | y >= 10 && y < 100 -> "10<= < 100"
        | otherwise -> "large"
    where y = x * 10

isIncreaseSeq :: (Ord a) => [a] -> Bool
isIncreaseSeq xs = 
    let impl [] = True
        impl (x:[]) = True
        impl (x:y:xs) = if x <= y then impl (y:xs)
                                  else False
    in impl xs
b1 = isIncreaseSeq [1, 2, 3]
b2 = isIncreaseSeq [1, 3, 2]
b3 = isIncreaseSeq [1]

intToStr :: Integer -> String
intToStr x 
    | x == 0 = "zero"
    | x == 1 = "one"
    | x == 2 = "two"
    | x == 3 = "three"
    | x == 4 = "four"
    | x == 5 = "five"
    | x == 6 = "six"
    | x == 7 = "seven"
    | x == 8 = "eight"
    | x == 9 = "nine"
    | otherwise = "???"

charToUpper :: Char -> Char
charToUpper c
    | isLower c = toUpper c
    | otherwise = c
    where 
        isLower c = fromEnum c >= fromEnum 'a' && fromEnum c <= fromEnum 'z'
        toUpper c = toEnum . (+ (fromEnum 'A' - fromEnum 'a')) . fromEnum $ c

strToUpper :: String -> String
strToUpper s = map charToUpper s

map1Time :: (a -> b) -> [a] -> [b]
map1Time _ [] = []
map1Time f (x:xs) = f x : map1Time f xs

map2Time :: (b -> c) -> (a -> b) -> [a] -> [c]
map2Time _ _ [] = []
map2Time f g (x:xs) = (f . g $ x) : map2Time f g xs

map2arity :: (a -> b -> c) -> [a] -> [b] -> [c]
map2arity _ _ [] = []
map2arity _ [] _ = []
map2arity f (x:xs) (y:ys) = f x y : map2arity f xs ys

numStrList = map1Time intToStr [1, 3, 10]
numStrList2 = map2Time strToUpper intToStr [1, 5, 0, 10, -1]
numStrList3 = let strToUpperImpl = map charToUpper in map2Time strToUpperImpl intToStr [1, 5, 0, 10, -1]

numStrList4 = map2arity (+) [1, 2] [3, 4, 5]

filter1Time :: (a -> Bool) -> [a] -> [a]
filter1Time _ [] = []
filter1Time f (x:xs) = if f x then x : filter1Time f xs
                              else filter1Time f xs
num5 = filter1Time (>5) [1, 6, 10]

ffm :: (a -> b -> c) -> (a -> Bool) -> (b -> Bool) -> [a] -> [b] -> [c]
ffm _ _ _ _ [] = []  
ffm _ _ _ [] _ = []
ffm m f1 f2 (x:xs) (y:ys)
    | f1 x && f2 y = m x y : ffm m f1 f2 xs ys
    | otherwise = ffm m f1 f2 xs ys
num6 = ffm (*) (\x -> x `rem` 2 == 0) (\x -> x `rem` 3 == 0) [1, 2, 3, 4] [3, 6, 1, 5]

foldLeft :: (b -> a -> b) -> b -> [a] -> b
foldLeft _ ini [] = ini
foldLeft f ini (x:xs) = foldLeft f (f ini x) xs

foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight _ ini [] = ini
foldRight f ini (x:xs) = f x (foldRight f ini xs)

str1 = foldLeft (++) "" ["a", "b", "c"]
str2 = foldRight (++) "" ["a", "b", "c"]

mapByFold :: (a -> b) -> [a] -> [b]
mapByFold f xs = foldRight ((:) . f) [] xs 

l2 = mapByFold (+1) [1, 2, 3]

lengthByFoldL :: [a] -> Int
lengthByFoldL xs = foldLeft (\acc _ -> acc + 1) 0 xs

lengthByFoldR :: [a] -> Int
lengthByFoldR xs = foldRight (\_ acc -> acc + 1) 0 xs

len1 = lengthByFoldL [1, 2, 3]
len2 = lengthByFoldR [1, 2, 3, 5, 6]
len3 = lengthByFoldR []

maxElem :: Ord a => [a] -> a
maxElem [] = error "empty list"
maxElem (x:xs) = foldLeft max x xs

max1 = maxElem [1, 4, 2, 8, 4, 5]

lastElemByFoldL :: [a] -> a
lastElemByFoldL [] = error "empty list"
lastElemByFoldL (x:xs) = foldLeft (curry snd) x xs  

num7 = lastElemByFoldL [1, 3, 5]

partialSumLeftImpl :: (a -> a -> a) -> a -> [a] -> [a]
partialSumLeftImpl f ini xs = ini : (
    case xs of
        [] -> []
        (x:xs) -> partialSumLeftImpl f (f ini x) xs
    )
partialSumLeft :: (a -> a -> a) -> a -> [a] -> [a]
partialSumLeft f ini xs = tail (partialSumLeftImpl f ini xs)

sum1 = partialSumLeft (+) 0 [1, 2, 3, 4]
pro1 = partialSumLeft (*) 1 [1, 2, 3, 4]

partialSumRightImpl :: (a -> a -> a) -> a -> [a] -> [a]
partialSumRightImpl _ ini [] = [ini]
partialSumRightImpl f ini (x:xs) = f x (head ys) : ys
    where
        ys = partialSumRightImpl f ini xs
partialSumRight :: (a -> a -> a) -> a -> [a] -> [a]
partialSumRight f ini xs = init (partialSumRightImpl f ini xs)

sum2 = partialSumRight (+) 0 [1, 2, 3, 4]
pro2 = partialSumRight (*) 1 [1, 2, 3, 4]

foldLByR :: (b -> a -> b) -> b -> [a] -> b
foldLByR f ini xs = foldr (\a g x -> g(f x a)) id xs ini

foldRByL :: (a -> b -> b) -> b -> [a] -> b
foldRByL f ini xs = foldl (\g a x -> g(f a x)) id xs ini

sum3 = foldLByR (+) 0 [1, 2, 3, 4]
sum4 = foldRByL (+) 0 [1, 2, 3, 4]

{-------------------- test ---------------------}
main :: IO()
-- main = print(num1, num2, num3)

-- main = print(num4)

-- main = print l1

-- main = print num5

-- main = print(inRange (-1), inRange 0.5, inRange 1.2, inRange 20)

-- main = print(b1, b2, b3)

-- main = print numStrList
-- main = print numStrList2
-- main = print numStrList3
-- main = print numStrList4

-- main = print num5
-- main = print num6

-- main = print(str1, str2)

-- main = print l2

-- main = print(len1, len2, len3)

-- main = print max1

-- main = print num7

-- main = print(sum1, pro1)
-- main = print(sum2, pro2)

main = print(sum3, sum4)
