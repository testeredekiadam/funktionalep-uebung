type Spielfeld = [Int]

permsN :: Int -> [[Int]]
permsN n = perms [1..n]

perms :: Eq a => [a] -> [[a]]
perms [a] = [[a]]
perms xs = [(x:c) | x <- xs, c <- perms $ filter (/=x) xs]

projectPos :: Int -> Int -> [Int]
projectPos x o = [(x+o),(x+o+o)..]

projectPos' x o = map(\x' -> x + o*x')[1..]--

noDiagonalHit :: Int -> Int -> Spielfeld -> Bool
noDiagonalHit x o l = null $ filter (\(x,y) -> x==y) $ zip l (projectPos x o)

noDiagonalHit' x o l = null $ filter (uncurry(==)) $ zip l (projectPos x o)--

queens :: Spielfeld
queens = queensN 8

queensN :: Int -> Spielfeld
queensN n = head (allQueens n)

allQueens :: Int -> [Spielfeld]
allQueens n = [sol | sol <- perms [1..n]]