import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as May
import Control.Monad

--Defining The Factorial nCr  = n!/r!(n-r)!
factorial' :: (Integral a) => a -> a -> a
factorial' _ 0      = 1
factorial' 0 _      = 0
factorial' n k      = factorial' (n-1) (k-1) * n `div` k 

-- defining the function which returns all the possible combinations, 
-- I'm explicitly assuming the k =  5 colors here 
sumFinder' :: (Integral a) => a -> a -> Maybe a
sumFinder' 0 _         = Nothing
sumFinder' _ 0         = Nothing
sumFinder' n  m        = Just $ Map.foldrWithKey f 0 (getMap' n m) 
                            where 
                            f k a len = len + ((folder' k n)*a)

--generating the unique mapfrp
getMap' ::   (Integral a) => a-> a -> Map.Map [a] a
getMap' n  m        = Map.fromListWith (+) $ zip (getAllSums' m n) (cycle [1])


getAllSums' :: (Integral a) =>  a -> a -> [[a]]
getAllSums' m  n = [ List.sort [a,b,c,d,e] | a <- [1..n], b <- [1..n], c <- [1..n], d <- [1..n], e <- [1..n], a+b+c+d+e == m]

folder' :: (Integral a) =>  [a] -> a -> a
folder' arr n = List.foldl (*) 1 $ map (factorial' n) arr

toFractional' :: (Num b, Integral a) =>  Maybe a -> b
toFractional' m = fromIntegral $ (May.fromJust m)

probability' :: (Integral a, Fractional b) => a -> a -> b 
probability' n m  = x/y 
                    where
                    x = toFractional' (sumFinder' n m)
                    y = fromIntegral (factorial' (5*n) m)
                    
getAllProbabilities' :: (Integral a, Fractional b, Ord b) => a -> b -> [(a,b)]
getAllProbabilities' n lim = genString' n [5..(n*5)] lim
  
genString' :: (Integral a, Fractional b, Ord b) => a -> [a] -> b -> [(a,b)]
genString' n (x:xs) lim
  | (probability' n x) > lim  =  [(x ,(probability' n x))]
  | otherwise =  [(x ,(probability' n x))] ++ (genString' n xs lim)
  

main = putStrLn $ show (getAllProbabilities' 10 0.9)
