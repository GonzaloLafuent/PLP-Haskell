import Data.Sequence.Internal.Sorting (QList(Nil))
import Data.Fixed (showFixed)
{-1B-}
max2 :: Float -> Float -> Float
max2 x y | x > y =  x
         | otherwise = y

normaVectorial :: Float -> Float -> Float
normaVectorial x y = sqrt (x^2 + y^2)         

{-2 (queda el III) -}
curry ::((a,b) -> c) -> a -> b -> c
curry f a b = f (a,b)

unCurry ::(a-> b-> c) -> (a,b) -> c
unCurry f (a,b) = f a b

{-3-}
{-a-}
sum_2::[Integer] -> Integer 
sum_2 l = foldr (+) 0 l 

elem_2 :: Eq a=> a -> [a] -> Bool
elem_2 e l = foldr (\x r -> (x==e) || r) False l 

concat_2 :: [a] -> [a] -> [a]
concat_2 a b = foldr (\x r-> x:r) b a

filter_2 :: (a->Bool) -> [a] -> [a]
filter_2 p l = foldr( \x r -> if p x then x:r else r) [] l 

map_2 :: (a -> b) -> [a] -> [b]
map_2 f = foldr(\x r -> (f x):r) [] 

{-b-}
mejor_segun :: (a -> a -> Bool) -> [a] -> a
mejor_segun p l = foldr1 (\x r-> if p x r then x else r ) l

{-c-}
sumas :: Num a => [a] -> [a]
sumas [] = []
sumas (x:xs) = [x + (sum xs)] ++ sumas xs

sumasParciales :: Num a => [a] -> [a]
sumasParciales l = reverse( sumas (reverse l))

{-d-}
sumaAlt :: Num a => [a] -> a
sumaAlt l = foldr (-) 0 l

{-
        foldr (-) 0 [1,2,3,4] = (-) 1 foldr (-) 0 [2,3,4]
                              = (-) 1 ((-) 2 foldr (-) 0 [3,4])
                              = (-) 1 ((-) 2 ((-) 3 foldr (-) 0 [4]))
                              = (-) 1 ((-) 2 ((-) 3 ((-) 4 foldr (-) 0 [])))
                              = (-) 1 ((-) 2 ((-) 3 ((-) 4 0)))
                              = (-) 1 ((-) 2 ((-) 3 (4-0)))
                              =  1 - ( 2 -( 3 - (4-0)))
                              =  1 - ( 2 - 3 + 4)
                              =  1 - 2 + 3  - 4 = -2  
        (Diapo 20 de la teorica 1 ejemplifica bien de acuerdo a como se asocian las operaciones)                      
-}
{-e-}
sumaAltInverse :: Num a => [a] -> a
sumaAltInverse l = foldr (-) 0 (reverse l)  

{-4-}
{-a-}
crearPermutacion :: (a -> [b]) -> [a] -> [b]
crearPermutacion f l  = [] 

permutaciones :: [a] -> [[a]]
permutaciones [] = []
permutaciones (x:xs) = []

{-c-}
prefijos :: [a] -> [[a]]
prefijos (x:xs) = []  

{-6-}
{- Esquema de recursion sobre listas -}
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

{-a-}
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e [] = []
sacarUna e (x:xs) = if x==e then xs else x:sacarUna e xs 

{-c-}
{-
insertaOrdenado :: Ord a => a -> [a] -> [a]
insertaOrdenado e [] = []
insertaOrdenado e (x:xs) = if e > x && null xs then x:[e] 
                           else if e < x && 
-} 

{-7-}
{-a-}
{-a = elemento inicial, (a->a) = funcion que da el sigueinte, Integer = cantidad de elementos , [a] = array que devuelve-}
genLista :: a -> (a -> a) -> Integer -> [a]
genLista e f 0 = []
genLista e f n = e:genLista (f e) f (n-1) 

{-b-}
{-x simepre es menor que y-}
desdeHasta ::  Integer -> Integer -> [Integer]
desdeHasta x y = genLista x (+1) ((y-x) +1) 

{-8-}
{-a-}
mapPares :: [(a,b)] -> (a -> b -> c) -> [c] 
mapPares [] f  = []
mapPares (x:xs) f = unCurry f x : mapPares xs f

{-b-}
armarPares :: [a] -> [b] -> [(a,b)]
armarPares [] a = []
armarPares b [] = []
armarPares (x:xs) (y:ys) = (x,y) : armarPares xs ys  

{-c-}
mapDoble :: (a->b->c) -> [a] -> [b] -> [c]
mapDoble f [] [] = []
mapDoble f (x:xs) (y:ys) = f x y : mapDoble f xs ys

{-9-}
{-a-}
sumaMat :: [[Integer]] -> [[Integer]] -> [[Integer]]
sumaMat [] [] = []
sumaMat (x:xs) (y:ys) = zipWith (+) x y : sumaMat xs ys 

{-b-}
trasponer :: [[Integer]] -> [[Integer]] 
trasponer [] = []
{-queda para despúes-}

{-10-}
generate :: ([a] -> Bool) -> ([a] -> a) -> [a]
generate stop next = generateFrom stop next []

generateFrom:: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom stop next xs | stop xs = init xs
                          | otherwise = generateFrom stop next (xs ++ [next xs])
{-
{-a-}
generateBase::([a] -> Bool) -> a -> (a -> a) -> [a]
generateBase stop e next | stop e = 
                         | otherwise = generateFrom stop nex (xs ++ [next xs])
{-b-}
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

factoriales :: Integer -> [Integer]
factoriales n = generate (\l -> not(null l) ) (\l -> map +1 l) 
-}

{-11-}
{-a-}
foldNat :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer
foldNat f x 0 = x
foldNat f x n = f x (foldNat f x (n-1))

{-b-}
potencia :: Integer -> Integer -> Integer
potencia x y = foldNat (\y res -> x * res) 1 y

{-12-}
data Polinomio a = X
                  |Cte a
                  |Suma (Polinomio a) (Polinomio a)
                  |Prod (Polinomio a) (Polinomio a)

foldPol :: b -> (a -> b) -> (b -> b ->b) -> (b -> b -> b) -> Polinomio a -> b
foldPol cX cCte cSuma cProd pol = case pol of
                                     x -> cX
                                     Cte k -> cCte k 
                                     Suma p q -> cSuma (r p) (r q)
                                     Prod p q -> cProd (r p) (r q)
                                   where r = foldPol cX cCte cSuma cProd    
 
evaluar :: Num a => a -> Polinomio a -> a
evaluar e p = foldPol e id (+) (*) p    
 