module U6 where
  import Data.Char


  let2int :: Char -> Int
  let2int = \a -> fromEnum a - 97

  int2let :: Int -> Char
  int2let = \a -> toEnum (a + 97)

  shift :: Int -> Char -> Char
  shift n a
    | isLower a == True = int2let((let2int a + n) `mod` 26)
    | otherwise = a

  encode :: Int -> String -> String
  encode n xs = [(shift n) x | x <- xs]

  encode' :: Int -> String -> String
  encode' n xs = map (shift n) xs

  encode2 :: Int -> (String -> String)
  encode2 n = map (shift n)

  encode3 :: Int -> String -> String
  encode3 n = map $ shift n


  rotate :: Int -> [a] -> [a]
  rotate n a = drop n' a ++ take n' a
    where n' = n `mod` (length a)

  count :: Eq a => a -> [a] -> Int
  count x xs = length[x' | x' <- xs, x' ==x ]

  count' :: Eq a => a -> [a] -> Int
  count' x xs = length $ filter (==x) xs

  count'' :: Eq a => a -> [a] -> Int
  count'' x = length . filter (==x)

  positions :: Eq a => a -> [a] -> [Int]
  positions x xs = [ n | n <- xs, n == x]


  














  percent :: Int -> Int -> Float
  percent n m = (fromIntegral n / fromIntegral m) * 100