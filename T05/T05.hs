double = \x -> x+x

sqr = \x -> x * x

gra = [\x -> \y -> x + y , \x -> \y -> x-y , \x -> \y -> x*y , \x -> \y -> x/y]

gra' = [(+), (-), (*), (/)]

gz = [x | x <- [1..20], x `mod` 2 == 0 ]

gz' s e d = [ x | x <- [s..e], x `mod` d == 0]

teiler n = [x | x <- [1..n], n `mod` x == 0]

skalarprodukt a b = sum[(ai * bi) | (ai,bi)  <- zip a b]

pyths n = [(x,y,z) | x<-[1..n], y <- [1..n], z <- [1..n], z*z == x*x + y*y ]

pyths' n = [(x,y,z) | x<-[1..n], y <- [1..n], z <- [1..n], z*z == x*x + y*y , y > x ]

-- [(x,y,z) | x<-[1..n], y <- [x..n], z <- [y..n], z*z == x*x + y*y ] geht auch


applyAll :: a -> b -> [a -> b -> c] -> [c]
applyAll x y fs = [f x y | f <- fs]


merge :: Ord a=>[a]->[a]->[a]

merge [] s = s
merge s [] = s
merge (a:as) (b:bs)
  | a <= b = a : merge as (b:bs)
  | otherwise = b : merge (a:as) bs