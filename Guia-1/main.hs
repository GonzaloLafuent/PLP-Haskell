import Data.Sequence.Internal.Sorting (QList(Nil))
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
filter_2 p l = foldr( 
                    \x r -> if p x then x:r
                            else r      
                ) [] l

map_2 :: (a -> b) -> [a] -> [b]
map_2 f l = foldr(\x r -> (f x):r) [] l

{-a-}
mejor_segun :: (a -> a -> Bool) -> [a] -> a
mejor_segun p l = foldr1 (\x r->) Nil l