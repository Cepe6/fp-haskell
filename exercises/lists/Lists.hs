{-# OPTIONS_GHC -Wall #-}

module Lists where

import qualified Data.List ()

import Prelude hiding (
        head, tail, null, length, reverse, repeat, replicate,
        concat, sum, maximum, take, drop, elem, (!!)
    )


head :: [Int] -> Int
head []    = error "empty list"
head (x:_) = x


tail :: [Int] -> [Int]
tail []     = error "empty list"
tail (_:xs) = xs


append :: [Int] -> [Int] -> [Int]
append []     ys = ys
append (x:xs) ys = x : (append xs ys)


elementAt :: Int -> [Int] -> Int
elementAt 0 (x:_) = x
elementAt _ []     = error "index greater than length"
elementAt n (_:xs) = elementAt (n - 1) xs


null :: [Int] -> Bool
null [] = True
null _  = False

length :: [Int] -> Int
length [] = 0
length (_:xs) = 1 + length xs


take :: Int -> [Int] -> [Int]
take _ [] = []
take 1 (x:_) = [x]
take n _ | n < 1 = []
take n (x:xs) = x : take (n-1) xs

take' :: Int -> [Int] -> [Int]
take' 0 _  = []
take' _ [] = []
take' n (x:xs) | n < 0     = []
               | otherwise = x : take' (n-1) xs


drop :: Int -> [Int] -> [Int]
drop _ [] = []
drop n (x:xs)
    | n >= (length (x:xs)) = []
    | n <= 0 = (x:xs)
    | otherwise = drop (n - 1) xs


elem :: Int -> [Int] -> Bool
elem _ [] = False
elem n (x:xs)
    | n == x = True
    | length xs == 0 = False
    | otherwise = elem n xs


reverseHelper :: [Int] -> [Int] -> [Int]
reverseHelper acc []     = acc
reverseHelper acc (x:xs) = reverseHelper (x:acc) xs

reverse :: [Int] -> [Int]
reverse xs = reverseHelper [] xs

reverseStringHelper::String->String->String
reverseStringHelper acc [] = acc
reverseStringHelper acc (x:xs) = (reverseStringHelper (x:acc) xs)


reverseString :: String -> String
reverseString str = "This is the reversed string: " ++ (reverseStringHelper [] str)

concat :: [[Int]] -> [Int]
concat [] = []
concat (x:xs) = append x (concat xs)

replicate :: Int -> Int -> [Int]
replicate 0 _ = []
replicate rep el
    | rep < 0 = []
    | otherwise = el : replicate (rep - 1) el


interleave :: [Int] -> [Int] -> [Int]
interleave [] _ = []
interleave (x:_) [] = [x]
interleave (x:xs) (x':xs') = x : x' : interleave xs xs'


sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

maximumHelper :: Int -> [Int] -> Int
maximumHelper acc [] = acc
maximumHelper acc (x:xs)
    | acc < x = maximumHelper x xs
    | otherwise = maximumHelper acc xs

maximum :: [Int] -> Int
maximum [] = error "empty list"
maximum (x:xs) = maximumHelper x xs


nub :: [Int] -> [Int]
nub [] = []
nub (x:xs)
    | elem x xs == False = x : nub xs
    | otherwise = nub xs

delete :: Int -> [Int] -> [Int]
delete _ [] = []
delete el (x:xs)
    | el /= x = x : delete el xs
    | otherwise = xs


difference :: [Int] -> [Int] -> [Int]
difference [] _ = []
difference xs [] = xs
difference (x:xs) xs'
    | elem x xs' == True = difference xs (delete x xs')
    | otherwise = x : difference xs xs'

unionHelper :: Int -> [Int] -> [Int]
unionHelper _ [] = []
unionHelper n (x:xs)
    | n == x = unionHelper n xs
    | otherwise = x : unionHelper n (delete n xs)

union :: [Int] -> [Int] -> [Int]
union [] [] = []
union [] xs' = xs'
union xs [] = xs
union xs (x':xs')
    | elem x' xs == False = x' : union xs (unionHelper x' xs')
    | otherwise = union xs xs'


intersect :: [Int] -> [Int] -> [Int]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) xs'
    | elem x xs' == True = x : intersect xs xs'
    | otherwise = intersect xs xs'
