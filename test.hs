import Data.Foldable (foldMap)
import qualified Data.Map as M
import Data.SBV
import qualified Data.Set as Set
import Propositional
import Queens
import Debug.Trace

type Env = M.Map String SBool

envLookup :: Formula -> Env -> SBool
envLookup (Variable v) e
  = maybe (error $ "Var not found: " ++ show v) id (M.lookup v e)

solveExpr :: Formula -> IO AllSatResult
solveExpr e0 = allSat go

  where vs :: [String]
        vs = map (\ (Variable c) -> c) (variables . fnc $ fnc e0)

        go :: Predicate
        go
          = do syms <- mapM symbolic vs
               let env = M.fromList (zip vs syms)
               -- traceM (show env)
               interpret env e0

        interpret :: Env -> Formula -> Predicate
        interpret env expr
          = do let interp = interpret env
               case expr of
                   x@(Variable v) -> return (envLookup x env)
                   Not e -> sNot `fmap` interp e
                   e1 :& e2 -> do r1 <- interp e1
                                  r2 <- interp e2
                                  return (r1 .&& r2)
                   e1 :| e2 -> do r1 <- interp e1
                                  r2 <- interp e2
                                  return (r1 .|| r2)
                   e1 :=> e2 -> error "And so on"
                   e1 :<=> e2 -> error "And so on"

-- main :: IO ()
-- main = do
--        let expr = testParse
--        putStrLn $ "Solving expr: " ++ show expr
--        either (error . show) (print <=< solveExpr) expr

ejemploDelLibro
  = (Not (Variable "p") :| Not (Variable "q") :| Not (Variable "r") :|
       Variable "s")
      :& (Not (Variable "t") :| Not (Variable "w") :| Variable "r")
      :& (Variable "q")
      :& (Variable "t")
      :& (Variable "v")
      :& (Not (Variable "v") :| Not (Variable "r") :| Variable "p")
      :& (Not (Variable "v") :| Variable "w")

formulaInsatisfacible
  = (Variable "p" :| Variable "q") :& (Not (Variable "q") :| Variable "r") :&
      (Variable "p" :=> Variable "r")
      :& Not (Variable "r")

formulaSatisfacible
  = (Variable "p" :| Variable "q") :& (Not (Variable "q") :| Variable "r") :&
      (Variable "p" :=> Variable "r")
