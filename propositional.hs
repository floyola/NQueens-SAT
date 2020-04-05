{-# LANGUAGE UnicodeSyntax #-}

module Propositional where

import Data.List(nub)

data Formula = Variable String
             | Not Formula
             | Formula :&Formula
             | Formula :|Formula
             | Formula :=>Formula
             | Formula :<=>Formula
                 deriving (Ord, Eq)

type Literal = Formula

type Clausula = [Literal]

infixl 9 :&

infixl 9 :|

infixr 7 :=>

infixl 8 :<=>

instance Show Formula where
        show (Variable v) = show v
        show (Not p) = "¬"    ++ show p
        show (p :& q) = "(" ++ show p ++ " ∧ "     ++ show q ++ ")"
        show (p :| q) = "(" ++ show p ++ " ∨ "     ++ show q ++ ")"
        show (p :=> q) = "(" ++ show p ++ " → "     ++ show q ++ ")"
        show (p :<=> q) = "(" ++ show p ++ " ⟷ "      ++ show q ++ ")"

-- Convierte una fórmula de la Lógica Proposisional en
-- Forma Normal Negativa.
fnn :: Formula -> Formula
fnn expr@(Variable _) = expr
fnn expr@(Not (Variable _)) = expr
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

-- Convierte una fórmula de la Lógica Proposisional en
-- Forma Normal Conjuntiva.
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

-- Dada una fórmula proposisional en forma normal conjuntiva
-- devuelve una lista con las claúsulas que la forman.
clausulas :: Formula -> Clausula
clausulas (p :| q) = [p :| q]
clausulas (p :& q) = clausulas p ++ clausulas q
clausulas p = [p]

-- Dada una claúsula regresa la lista de las literales que la conforma.
literales :: Formula -> Clausula
literales var@(Variable _) = [var]
literales negVar@(Not _) = [negVar]
literales (p :| q) = literales p ++ literales q

-- Dada una fórmula regresa la lista de las variables que la conforma.
variables :: Formula -> Clausula
variables var@(Variable _) = [var]
variables (Not x) = variables x
variables (p :| q) = nub $ variables p ++ variables q
variables (p :& q) = nub $ variables p ++ variables q

-- Verifica que una clausula sea unitaria.
isUnit :: Clausula -> Bool
isUnit [p] = True
isUnit _ = False

-- Verifica que una literal este negada.
isNegLiteral :: Literal -> Bool
isNegLiteral (Not _) = True
isNegLiteral _ = False

-- Verifica que una clausula sea positiva.
isPositive :: Clausula -> Bool
isPositive c = all (not . isNegLiteral) c

--Dada una Literal regresa su complemento.
complemento :: Literal -> Literal
complemento var@(Variable _) = Not var
complemento (Not (Variable f)) = Variable f
