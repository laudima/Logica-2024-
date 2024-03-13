module FormulasProposicionales where

import Operadores
import Data.List

-- Representa variables p, q, r, s...
type Atom = String

-- Representa las variables que se evalúan a True.
type State = [Atom]

-- data Prop
data Prop = Var Atom
          | Neg Prop
          | Conj Prop Prop
          | Disy Prop Prop
          | Impl Prop Prop
          | Syss Prop Prop

-- instance Show
instance Show Prop where
  show (Var p) = show p
  show (Neg p) = "¬" ++ show p
  show (Conj p q) = show p ++ "/\\" ++ show q
  show (Disy p q) = show p ++ "\\/" ++ show q
  show (Impl p q) = show p ++ "-->" ++ show q
  show (Syss p q) = show p ++ "<-->" ++ show q

instance Operadores Prop where
  (¬) p = Neg p
  (/\) p q = Conj p q
  (\/) p q = Disy p q
  (-->) p q = Impl p q
  (<-->) p q = Syss p q
---------------------------------------------------------------------------------
--------                             FUNCIONES                           --------
---------------------------------------------------------------------------------

-- Funcion que dada una formula, regresa el conjunto de todos los
-- símbolos que aparecen en ella. 
vars :: Prop -> [Atom]
vars (Var p) = [p]
vars (Neg p) = vars p
vars (Conj p q) = vars p ++ vars q
vars (Disy p q) = vars p ++ vars q 
vars (Impl p q) = vars p ++ vars q 
vars (Syss p q) = vars p ++ vars q 

-- Funcion que evalua una proposicion dado un estado.
interp :: State -> Prop -> Bool
interp = error "D:"

{-
State = ["p"]
Prop  = Conj (Var "p") (Var "q")
-}

-- Funcion que elimina las equivalencias (<->).
elimEquiv :: Prop -> Prop
elimEquiv = error "D:"

-- Funcion que elimina las implicaciones, puedes suponer que no hay
-- equivalencias.
elimImpl :: Prop -> Prop
elimImpl = error "D:"

{-
P -> (Q -> R) => ¬P v (Q -> R) => ¬P v (¬Q v R)
-}

-- Funcion que da TODAS las posibles interpretaciones que podria tomar
-- una formula.
posiblesInterp :: Prop -> [State]
posiblesInterp = error "D:"

-- Funicion que nos dice si un estado es modelo de una proposicion.
esModelo :: Prop -> State -> Bool
esModelo = error "D:"

-- Funcion que nos da TODOS los modelos de una proposicion.
todosModelos :: Prop -> [State]
todosModelos = error "D:"

-- Funcion que nos dice si una proposicion es satifacible.
esSatisfacible :: Prop -> Bool
esSatisfacible = error "D:"

-- Funcion que nos dice si una proposicion es instisfacible.
esInsatisfacible :: Prop -> Bool
esInsatisfacible = error "D:"

-- Funcion que nos dice si una proposicion es una tautologia.
esTautologia :: Prop -> Bool
esTautologia = error "D:"

-- Funcion que nos dice si una proposicion es una contradiccion.
esContradiccion :: Prop -> Bool
esContradiccion = error "D:"

---------------------------------------------------------------------------------
--------                           AUXILIARES                            --------
---------------------------------------------------------------------------------

potencia :: [a] -> [[a]]
potencia [] = [[]]
potencia (x:xs) = [ x:ys | ys <- xss] ++ xss
  where
    xss = potencia xs

---------------------------------------------------------------------------------
--------                             EJEMPLOS                            --------
---------------------------------------------------------------------------------

i  = Conj (Var "p") (Var "q")
i' = (Var "p") /\ (Var "q")

p = Var "p"
q = Var "q"
r = Var "r"
varsrp = ["r", "p"]
form1 = ((p \/ q) /\ (((¬) q) \/ r))
interp1 = interp varsrp form1

taut1 = (p \/ (¬) p)
taut2 = ((p \/ q) \/ ((¬)p /\ (¬) q))

cont1 = ((p \/ q) /\ ((¬)p /\ (¬) q))

potencia1 = potencia [1,2,3]
