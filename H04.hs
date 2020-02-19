{- Aufgabe 1 -}
-- 1.)
oder :: Bool -> Bool -> Bool
oder a b = a || b

-- 2.)
fst3 :: (a, b ,c ) -> a
fst3 (a, _, _) = a

-- 3.)
oneOfFour :: Int -> (a,a,a,a) -> a
oneOfFour n (a, b, c, d)
  | n == 1 = a
  | n == 2 = b
  | n == 3 = c
  | n == 4 = d
  | otherwise = error "1-4"

-- 4.)
countNumOccurences c [] = 0
countNumOccurences c (e:es) = if e == c then 1 + countNumOccurences c es else countNumOccurences c es


{- Aufgabe 2 -}
-- 1.)
ggt :: (Int, Int) -> Int
ggt (0, 0) = error "Nicht defininiert"
ggt (0, y) = y
ggt (x,y) = ggt(y `mod` x, x)

-- 2.)
kuerzen :: (Int, Int) -> (Int, Int)
kuerzen (x,y) = (x `div` ggt(x,y), y `div` ggt(x,y))

kuerzenWhere :: (Int, Int) -> (Int, Int)
kuerzenWhere (x,y) = (x `div` g, y `div` g) where g = ggt(x,y)

kuerzenLet :: (Int, Int) -> (Int, Int)
kuerzenLet (x,y) = 
    let g = ggt(x,y)
    in (x `div` g, y `div` g)
