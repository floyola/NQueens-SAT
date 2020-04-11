module Queens where

import Propositional

----------------------------------------
----------------------------------------

exactlyOne :: [Literal] -> Formula
exactlyOne vars = (foldr1 (:&) . map (\(p,q) -> Not(p) :| Not(q)) $  pairs vars)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map ((,) x) xs ++ pairs xs

diagonals :: Int -> [[Literal]]
diagonals n = [[ Var $ chessSquare x y | x <- [1..n], y <- [1..n], x-y == t] | t <- [-bound..bound]]
  where bound = n-2

antiDiagonals :: Int -> [[Literal]]
antiDiagonals n = [[ Var $ chessSquare x y | x <- [1..n], y <- [1..n], x+y == t] | t <- [3..bound]]
  where bound = 2*n-1

columns :: Int -> [[Literal]]
columns n = [[Var $ chessSquare x y | x <- [1..n]] | y <- [1..n]]

rows :: Int -> [[Literal]]
rows n = [[Var $ chessSquare x y | y <- [1..n]] | x <- [1..n]]

chessSquare :: Int -> Int -> String
chessSquare n m = "square_" ++ show n ++ "_" ++ show m

----------------------------------------
----------------------------------------

queensToSat :: Int -> Formula
queensToSat n = foldr1 (:&) columnas :& (foldr1 (:&) renglones) :& (foldr1 (:&) diagonales) :& foldr1 (:&) atLeast
  where columnas = map exactlyOne $ columns n
        atLeast = map (foldr1 (:|)) $ columns n
        renglones = map exactlyOne $ rows n
        diagonales = (map exactlyOne $ diagonals n) ++ (map exactlyOne $ antiDiagonals n)
