module Escaped where

import Data.List

numberOfCharactersEscaped expression = findBrackets expression 0

checkList :: String
checkList = ['a'..'z']

findBrackets :: Num a => String -> a -> a
findBrackets [] acc = acc
findBrackets (x:xs) acc | x == '#' = findEscaped xs acc 
                        | otherwise = findBrackets xs acc

findEscaped :: Num a => String -> a -> a
findEscaped [] acc = acc
findEscaped (x:xs) acc | x == '#' = findBrackets xs acc
                       | x == '!' = if Data.List.head xs `elem` checkList 
                                    then findEscaped (Data.List.tail xs) (acc + 1) 
                                    else findEscaped xs acc
                       | otherwise = findEscaped xs acc