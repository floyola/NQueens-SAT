module Queens where

import Propositional

----------------------------------------
----------------------------------------

-- Returns all possible pairs without symmetries
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map ((,) x) xs ++ pairs xs

-- Given a set of variables construct a formula that corresponds to the *maxOne* SAT constraint.
maxOne :: [Literal] -> Formula
maxOne = foldr1 (:&) . map (\(p,q) -> Not(p) :| Not(q)) .  pairs

-- Returns lists of variables that correspond to the diagonals of the chessboard
encodeDiagonals :: Int -> [[Literal]]
encodeDiagonals n = [[ Var(chessSquare x y) | x <- [1..n], y <- [1..n], x-y == t] | t <- [-bound..bound]]
  where bound = n-2

-- Returns lists of variables that correspond to the anti-diagonals of the chessboard
encodeAntiDiagonals :: Int -> [[Literal]]
encodeAntiDiagonals n = [[ Var(chessSquare x y) | x <- [1..n], y <- [1..n], x+y == t] | t <- [3..bound]]
  where bound = 2*n-1

-- Returns lists of variables that correspond to the columns of the chessboard
encodeColumns :: Int -> [[Literal]]
encodeColumns n = [[Var(chessSquare x y) | x <- [1..n]] | y <- [1..n]]

-- Returns lists of variables that correspond to the rows of the chessboard
encodeRows :: Int -> [[Literal]]
encodeRows n = [[Var(chessSquare x y) | y <- [1..n]] | x <- [1..n]]

chessSquare :: Int -> Int -> String
chessSquare n m = "square_" ++ show n ++ "_" ++ show m

----------------------------------------
----------------------------------------

-- Encode the NQueens Problem into a Propositional Formula
queensToSat :: Int -> Formula
queensToSat n = foldr1 (:&) columns
                :& foldr1 (:&) rows
                :& foldr1 (:&) diagonals
                :& foldr1 (:&) antiDiagonals
                :& foldr1 (:&) atLeastOne

          where columns = map maxOne $ encodeColumns n
                rows = map maxOne $ encodeRows n
                diagonals = map maxOne $ encodeDiagonals n
                antiDiagonals = map maxOne $ encodeAntiDiagonals n
                -- We need to guarantee that there'll be at at least one queen per column
                atLeastOne = map (foldr1 (:|)) $ encodeColumns n
