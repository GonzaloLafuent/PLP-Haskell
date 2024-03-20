import Data.Maybe
import Data.Either
import Data.String

{-
  Punto 2 (falta el d)  
-}

valorAbsoluto :: Float -> Float
valorAbsoluto x | x<0 = -(x)
                |otherwise = x


esBisiesto :: Integer -> Bool
esBisiesto x | x `mod` 400 == 0 = True
             | x `mod` 100 /= 0 && x `mod` 4 == 0 = True
             | otherwise = False

factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial(x-1)

{-
  Punto 3 (problema para probar el b)  
-}

inverso :: Float -> Maybe Float
inverso x | (1/x) * x == 1 = Just (1/x)
          | otherwise = Nothing  

aEntero :: Either Integer Bool -> Integer
aEntero (Right x) | x = 1
                  |otherwise = 0
aEntero (Left y) = y

{-
    Punto 4
-}

s
limpiar :: [Integer] -> [Integer] 
limpiar x = filter (<5) x
    