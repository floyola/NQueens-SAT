{-# LANGUAGE UnicodeSyntax #-}

module Propositional where

import Data.List(nub)

----------------------------------------
----------------------------------------


data Formula = Var String
             | Not Formula
             | Formula :& Formula
             | Formula :| Formula
             | Formula :=> Formula
             | Formula :<=> Formula
                 deriving (Ord, Eq)

instance Show Formula where
       show (Var v) = show v
       show (Not p) = "¬"    ++ show p
       show (p :& q) = "(" ++ show p ++ " ∧ "     ++ show q ++ ")"
       show (p :| q) = "(" ++ show p ++ " ∨ "     ++ show q ++ ")"
       show (p :=> q) = "(" ++ show p ++ " → "     ++ show q ++ ")"
       show (p :<=> q) = "(" ++ show p ++ " ⟷ "      ++ show q ++ ")"

type Literal = Formula
type Clausula = [Literal]

infixl 9 :&
infixl 9 :|
infixr 7 :=>
infixl 8 :<=>

----------------------------------------
----------------------------------------


-- Transform a formula p into Negative Normal Form
fnn :: Formula -> Formula
fnn expr@(Var _) = expr
fnn expr@(Not (Var _)) = expr
fnn (Not (Not expr)) = expr

fnn (exp1 :& exp2) = fnn exp1 :& fnn exp2
fnn (Not (exp1 :& exp2)) = fnn $ Not exp1 :| Not exp2

fnn (exp1 :| exp2) = fnn exp1 :| fnn exp2
fnn (Not (exp1 :| exp2)) = fnn $ Not exp1 :& Not exp2

fnn (exp1 :=> exp2) = fnn $ Not exp1 :| exp2
fnn (Not (exp1 :=> exp2)) = fnn $ exp1 :& Not exp2

fnn (exp1 :<=> exp2)
  = let a = exp1 :& exp2
        b = Not exp1 :& Not exp2
      in fnn $ a :| b
fnn (Not (exp1 :<=> exp2))
  = let a = exp1 :| exp2
        b = Not exp1 :| Not exp2
      in fnn $ a :& b

-- Transform a formula p into Conjunctive Normal Form
fnc :: Formula -> Formula
fnc = fnc' . fnn
  where fnc' :: Formula -> Formula
        fnc' (exp1 :& exp2) = fnc' exp1 :& fnc' exp2
        fnc' (exp1 :| exp2) = fnc' exp1 `dist` fnc' exp2
        fnc' expr = expr

        dist :: Formula -> Formula -> Formula
        dist (e11 :& e12) e2 = (dist e11 e2) :& (dist e12 e2)
        dist e1 (e21 :& e22) = (dist e1 e21) :& (dist e1 e22)
        dist e1 e2 = e1 :| e2

-- Given a formula returns the list of variables that compose it.
variables :: Formula -> Clausula
variables var@(Var _) = [var]
variables (Not x) = variables x
variables (p :| q) = nub $ variables p ++ variables q
variables (p :& q) = nub $ variables p ++ variables q
