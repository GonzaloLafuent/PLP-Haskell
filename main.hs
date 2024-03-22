import Data.Maybe
import Data.Either
import Data.String
import GHC.Exts.Heap (GenClosure(value))

{-
  Punto 2 (falta el d)  
-}

valorAbsoluto :: Float -> Float
valorAbsoluto x | x<0 = -x
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

{-
  dame dato solo como prueba para obtener un valor Either Integer Bool
-}
dameDato :: Integer -> Either Integer Bool
dameDato x | x>0 = Right True
           | otherwise = Left x

aEntero :: Either Integer Bool -> Integer
aEntero (Right x) | x = 1
                  |otherwise = 0
aEntero (Left y) = y

{-
    Punto 4

    Prueba de filter con array de numeros:
      limpiar :: a -> [Integer] 
      limpiar x =  filter (>5) (numArray x)

-}

word1 = \x -> ["p","u","e","r","t","a"]
word2 = \x -> ["p","e","r","a"]
numArray = \x -> [2,5,6,5]

char = \x -> 'a'
 
estaLaLetra :: Char -> Char -> Bool
estaLaLetra x y | x == y = False
            | otherwise = True   


limpiar :: String -> String  -> String
limpiar x y | x == y = x
            | otherwise = y
    