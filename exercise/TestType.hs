module TestType where

listLT :: (Ord a) => [a] -> [a] -> Bool
listLT x y = 
    case x of 
        [] -> case y of
                [] -> False
                (b:bs) -> True
        (a:as) -> case y of
                    [] -> False
                    (b:bs) -> case compare a b of
                                LT -> True
                                EQ -> listLT as bs
                                GT -> False

listEQ :: (Ord a) => [a] -> [a] -> Bool
listEQ x y = 
    case x of
        [] -> case y of
                [] -> True
                (b:bs) -> False
        (a:as) -> case y of
                    [] -> False
                    (b:bs) -> case compare a b of
                                EQ -> listEQ as bs
                                _ -> False

listNE :: (Ord a) => [a] -> [a] -> Bool
listNE x y = not (listEQ x y)

listLE :: (Ord a) => [a] -> [a] -> Bool
listLE x y = (listEQ x y) || (listLT x y)

listGT :: (Ord a) => [a] -> [a] -> Bool
listGT x y = listLT y x

listGE :: (Ord a) => [a] -> [a] -> Bool
listGE x y = (listEQ x y) || (listGT x y)

data IntList = IntList [Int]

class ListOrd a where
    eq, ne, lt, le, gt, ge :: a -> a -> Bool

    eq x y = not (x `ne` y)
    ne x y = not (x `eq` y)
    lt x y = y `gt` x 
    le x y = (x `eq` y) || (x `lt` y)
    gt x y = y `lt` x
    ge x y = not (x `lt` y)

instance ListOrd IntList where
    eq x y = 
        let IntList a = x
            IntList b = y
        in listEQ a b

    lt x y = 
        let IntList a = x
            IntList b = y
        in listLT a b

l1 = IntList [1, 2, 3]
l2 = IntList [1, 2, 3]
l3 = IntList [1, 3, 4]
l4 = IntList [1, 2, 3, 4]
l5 = IntList [1, 2, 0]
l6 = IntList [1, 2]
l7 = IntList [0, 2]

data Season = Spring | Summer | Autumn | Winter deriving (Enum, Show, Bounded)
seasonNames = [show Spring, show Summer, show Autumn, show Winter]
firstSeason = minBound :: Season
lastSeason = maxBound :: Season

data Point = Point Double Double deriving (Show)
instance Eq Point where
    p1 == p2 = 
        let Point x1 y1 = p1
            Point x2 y2 = p2
        in [x1, y1] == [x2, y2]
instance Ord Point where
    compare p1 p2 =
        let Point x1 y1 = p1
            Point x2 y2 = p2
        in compare [x1, y1] [x2, y2]

p1 = Point 3 4
p2 = Point 2 5
p3 = Point 2 5

newtype Month = Month Int deriving Eq
instance Show Month where
    show m 
        | m == Month 1 = "Jan"
        | m == Month 2 = "Feb"
        | m == Month 3 = "Mar"
        | m == Month 4 = "Apr"
        | m == Month 5 = "May"
        | m == Month 6 = "Jun"
        | m == Month 7 = "Jul"
        | m == Month 8 = "Aug"
        | m == Month 9 = "Sep"
        | m == Month 10 = "Oct"
        | m == Month 11 = "Nov"
        | m == Month 12 = "Dec"
        | otherwise = "???"
instance Enum Month where
    succ = toEnum . next . fromEnum
        where next n = case n of
                        12 -> 1
                        _ -> n + 1
    pred = toEnum . prev . fromEnum
        where prev n = case n of
                        1 -> 12
                        _ -> n - 1

    fromEnum m = 
        let Month x = m in x
    toEnum x = Month x
instance Bounded Month where 
    minBound = Month 1
    maxBound = Month 12

jan = Month 1
dec = Month 12

{------------------------------ test ---------------------------------}
main :: IO()
-- main = print(l1 `eq` l2, l1 `lt` l3, l1 `lt` l4, l1 `gt` l5, l1 `gt` l6, l1 `gt` l7, l2 `le` l3, l5 `ge` l6, l3 `ge` l4)

-- main = print(seasonNames, firstSeason, lastSeason)
-- main = print([Spring .. Winter])

-- main = print(p1, p2, p3, p1 > p2, p2 == p3)

main = print(jan, dec, succ jan, succ dec, pred jan, pred dec)