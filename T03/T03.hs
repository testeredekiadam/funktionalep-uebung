max2Floats :: (Float, Float) -> Float
max2Floats (a,b) =
  if a > b then a
  else b

min2Floats :: (Float, Float) -> Float
min2Floats (a,b) =
  if a<b then a
  else b

add2Floats :: (Float, Float) -> Float
add2Floats (a,b) = a+b

add3Floats :: (Float, Float, Float) -> Float
add3Floats (a,b,c) = a+b+c

mul2Floats :: (Float, Float) -> Float
mul2Floats (a,b) = a * b

mul3Floats :: (Float,Float,Float) -> Float
mul3Floats (a,b,c) = a * b * c 

add2FloatsC :: Float -> (Float -> Float)
add2FloatsC a b = a+b

add3FloatsC :: Float -> (Float -> (Float -> Float ))
add3FloatsC a b c = a + b + c



s :: Int -> Int
s a = a + 1 


lengthFloats :: [Float] -> Int
lengthFloats [] = 0
lengthFloats (e:es) = 1 + (lengthFloats es)

sumFloat :: [Float] -> Float
sumFloat [] = 0
sumFloat (e:es) = e +(sumFloat es)

productFloats :: [Float] -> Float
productFloats [] = 1
productFloats (e:es) = e * (productFloats es)

minimumFloats :: [Float] -> Float
minimumfloats [e] = e
minimumFloats (e:es) =
  if e < (minimumFloats es) then e
  else (minimumFloats es)