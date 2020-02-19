-- Aufgabe 1)
-- 1.)

oder :: Bool -> Bool -> Bool
oder True _ = True
oder _ _ = False

-- 2.)
fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

-- 3.)
oneOfFour :: Int -> (a,a,a,a) -> a
oneOfFour n (a,b,c,d)   | n==1 = a
                        | n==2 = b
                        | n==3 = c
                        | n==4 = d
                        | otherwise = error "Scheiße"

 -- 4.)
countOcc :: Ord a => a -> [a] -> Int
countOcc x xs = (length . filter (== x)) xs
-- Internet Lösung :(

countNumOccurrences :: Eq a => a -> [a] -> Int
countNumOccurrences a [] = 0
countNumOccurrences a (e:es)
    | a == e = 1+ (countNumOccurrences a es)
    | otherwise = (countNumOccurrences a es)


-- Aufgabe 2)
-- 1.)
ggt :: (Int, Int) -> Int
ggt (0,0) = error "Nicht_definiert"
ggt (x,y) | x==0 = y
          | y==0 = x
          | otherwise = myggt(y `mod` x, x)
