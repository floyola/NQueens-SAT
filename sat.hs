module SAT where

import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.SBV
import Propositional
import Queens

----------------------------------------
----------------------------------------

type Env = M.Map String SBool

-- Given a variable and an Env returns the associated SBool.
lookup :: Formula -> Env -> SBool
lookup (Var v) e = maybe (error $ "Var not found: " ++ show v) id (M.lookup v e)

-- Transform a Formula into a SBV.Predicate
formulaToPredicate :: Formula -> Predicate
formulaToPredicate expr = do syms <- mapM symbolic vars
                             let env = M.fromList (zip vars syms)
                              in toPredicate env expr
                          where vars = map (\(Var c) -> c) (variables expr)

-- Auxiliary function to transform a Formula into a SBV.Predicate
toPredicate :: Env -> Formula -> Predicate
toPredicate env expr
      = do let interp = toPredicate env
           case expr of
               x@(Var v) -> return (lookup x env)

               Not e -> sNot `fmap` interp e

               e1 :& e2 -> do r1 <- interp e1
                              r2 <- interp e2
                              return (r1 .&& r2)

               e1 :| e2 -> do r1 <- interp e1
                              r2 <- interp e2
                              return (r1 .|| r2)

----------------------------------------
----------------------------------------

-- Given an integer n solves the n queens problem using SAT.
solveQueens :: Int -> IO AllSatResult
solveQueens = allSat . formulaToPredicate . queensToSat
