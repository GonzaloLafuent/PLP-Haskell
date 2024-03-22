import Data.Maybe
import Data.Either
import Data.String
import GHC.Exts.Heap (GenClosure(value))

{-
  Punto 1

  null: funcion que devuelve true si una lista esta vacia
  head: devuelve el primer elemento de la lista
  tail: devuelve la lista sin el primer elemento
  init: devuelve los primeros elementos sin el ultimo
  last: devuelve el ultimo elemento de la lista
  take: recibe un entero n y un array y devuelve los primeros n elementos
  drop: recibe un entero n y un array y elimina los primeros n elementos
  elem: recibe un elemento e y un array y devuelve true si e esta en el array
  concat: recibe una lista de listas y las concatena
  (++): concatena dos listas
  (!!): ??
-}

{-
  Punto 2 
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

{-cantdivisoresprimos-}
esPrimo :: Integer -> Integer -> Bool
esPrimo 0 j = False
esPrimo 1 j = True
esPrimo y j | j > y = True
            | y `mod` j == 0 && y/=1 && y/=j = False
            | otherwise = esPrimo y (j+1)

divisoresPrimos :: Integer -> Integer -> Integer
divisoresPrimos y x | y == x && esPrimo x 2 = 1
                    | y == x = 0
                    | x `mod` y == 0 && esPrimo y 2 = 1 + divisoresPrimos(y+1) x
                    | otherwise = divisoresPrimos(y+1)x

cantDivisoresPrimos :: Integer -> Integer
cantDivisoresPrimos 0 = 0
cantDivisoresPrimos 1 = 1
cantDivisoresPrimos x = divisoresPrimos 1 x 

{-
  Punto 3   
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
    Falta el de limpiar
-}

word1 = \x -> ["p","u","e","r","t","a"]
word2 = \x -> ["p","e","r","a"]
numArray = \x -> [2,5,6,5]

char = \x -> 'a'
 
estaLaLetra :: Char -> Char -> Bool
estaLaLetra x y | x == y = False
                | otherwise = True   

{-Limpiar-}
limpiar :: String -> String  -> String
limpiar x y | x == y = x
            | otherwise = y

{-Dif Promedio-}    
suma :: [Float] -> Float
suma [] = 0
suma (x:xs) = x + suma xs

promedio :: [Float] -> Float    
promedio x = suma x / fromIntegral (length x) :: Float   

difPromedio :: [Float] -> [Float] 
difPromedio x = map (\y -> y - promedio x) x

{-Todos Iguales-}
iguales :: Integer -> [Integer] -> Bool
iguales y [] = True
iguales y (x:xs) | x == y = True && iguales y xs 
                 |otherwise = False

todosIguales :: [Integer] -> Bool
todosIguales [] = True
todosIguales (x:xs) = iguales x xs

{-
  Punto 5
-}

data AB a = Nil | Bin (AB a) a (AB a)
arbol_1 :: AB Integer = Nil
arbol_2 :: AB Integer = Bin (Bin Nil 2 Nil) 3 (Bin Nil 4 Nil )

arbol_3 :: AB Bool = Nil
arbol_4 :: AB Bool = Bin (Bin Nil True Nil) True (Bin Nil True Nil )

vacioAB :: AB a -> Bool 
vacioAB Nil = True
vacioAB x = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin l c r) = Bin (negacionAB l) (not c) (negacionAB r)

showAB :: AB a -> [a]
showAB Nil = []
showAB (Bin l c r) = showAB(l)++[c]++showAB(r)  

productoAB :: AB Integer -> Integer
productoAB Nil = 1
productoAB (Bin l c r) = c * productoAB(l) * productoAB(r)