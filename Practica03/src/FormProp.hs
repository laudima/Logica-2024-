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
  show (Neg p) =  "(" ++  "¬" ++ show p ++ ")"
  show (Conj p q) = "(" ++ show p ++ "/\\" ++ show q ++ ")"
  show (Disy p q) = "(" ++ show p ++ "\\/" ++ show q ++ ")"
  show (Impl p q) = "(" ++ show p ++ "->" ++ show q ++ ")"
  show (Syss p q) = "(" ++ show p ++ "<->" ++ show q  ++ ")"

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
-- símbolos que aparecen en ella. Utiliza la funcion union de Data.List. 
vars :: Prop -> [Atom]
vars (Var p) = [p]
vars (Neg p) = vars p
vars (Conj p q) = vars p `union` vars q
vars (Disy p q) = vars p `union` vars q
vars (Impl p q) = vars p `union` vars q
vars (Syss p q) = vars p `union` vars q

-- Funcion que evalua una proposicion dado un estado.
interp :: State -> Prop -> Bool
-- Si la proposicion es una variable, entonces regresa True si la variable esta en el estado.
interp s (Var p) = p `elem` s
-- Si la proposicion es una negacion, entonces regresa True si la negacion de la proposicion es falsa.
interp s (Neg p) = not (interp s p)
-- Si la proposicion es una conjuncion, entonces regresa True si ambas proposiciones son verdaderas.
interp s (Conj p q) = interp s p && interp s q
-- Si la proposicion es una disyuncion, entonces regresa True si al menos una de las proposiciones es verdadera.
interp s (Disy p q) = interp s p || interp s q
-- Si la proposicion es una implicacion, entonces regresa True si la proposicion es falsa o si ambas proposiciones son verdaderas.
interp s (Impl p q) = not (interp s p) || interp s q
-- Si la proposicion es una equivalencia, entonces regresa True si ambas proposiciones son verdaderas o si ambas son falsas.
interp s (Syss p q) = (interp s p && interp s q) || (not (interp s p) && not (interp s q))

{-
State = ["p"] * solo son variables 
Prop  = Conj (Var "p") (Var "q")
-}

-- Funcion que elimina las equivalencias (<->). * elimina solo el <-> por <- y ->
elimEquiv :: Prop -> Prop
elimEquiv (Var p) = Var p
elimEquiv (Neg p) = Neg (elimEquiv p)
elimEquiv (Conj p q) = Conj (elimEquiv p) (elimEquiv q)
elimEquiv (Disy p q) = Disy (elimEquiv p) (elimEquiv q)
elimEquiv (Impl p q) = Impl (elimEquiv p) (elimEquiv q)
elimEquiv (Syss p q) = Conj (Impl (elimEquiv p) (elimEquiv q)) (Impl (elimEquiv q) (elimEquiv p))

-- Funcion que elimina las implicaciones, puedes suponer que no hay
-- equivalencias.
elimImpl :: Prop -> Prop
elimImpl (Var p) = Var p
elimImpl (Neg p) = Neg (elimImpl p)
elimImpl (Conj p q) = Conj (elimImpl p) (elimImpl q)
elimImpl (Disy p q) = Disy (elimImpl p) (elimImpl q)
elimImpl (Impl p q) = Disy (Neg (elimImpl p)) (elimImpl q)
elimImpl (Syss p q) = error "D:"

{-
P -> (Q -> R) => ¬P v (Q -> R) => ¬P v (¬Q v R)
-}

-- Funcion que da TODAS las posibles interpretaciones que podria tomar * powerconjunto 
-- una formula.
posiblesInterp :: Prop -> [State]
posiblesInterp p = potencia (vars p)

-- Funicion que nos dice si un estado es modelo de una proposicion.
esModelo :: Prop -> State -> Bool
esModelo = flip interp 

-- Funcion que nos da TODOS los modelos de una proposicion.
-- Lista de listas de todos los estados donde la funcione es verdadera 
todosModelos :: Prop -> [State]
todosModelos p = filter (esModelo p) (posiblesInterp p)

-- Funcion que nos dice si una proposicion es satifacible.
esSatisfacible :: Prop -> Bool
esSatisfacible p = not (null (todosModelos p)) -- hay al menos una donde es verdadera 

-- Funcion que nos dice si una proposicion es instisfacible.
esInsatisfacible :: Prop -> Bool
esInsatisfacible p = not (esSatisfacible p) -- no hay ninguna donde es verdadera

-- Funcion que nos dice si una proposicion es una tautologia.
esTautologia :: Prop -> Bool
esTautologia p = (length (todosModelos p)) == (length (posiblesInterp p)) -- el tamaño de todos los modelos es igual de las posibles interpretaciones 

-- Funcion que nos dice si una proposicion es una contradiccion.
esContradiccion :: Prop -> Bool
esContradiccion p = not (esSatisfacible p) -- no hay ninguna donde es verdadera

-- Funcion que dada una formula f, regresa la forma normal negativa. 
formaNormalNegativa :: Prop -> Prop
formaNormalNegativa (Var p)               = Var p
formaNormalNegativa (Neg (Var   p))       = Neg (Var p)
formaNormalNegativa (Neg (Neg p))         = p

formaNormalNegativa (Conj p q)            = formaNormalNegativa p `Conj` formaNormalNegativa q
formaNormalNegativa (Neg (Conj p q))      = formaNormalNegativa (Neg p `Disy` Neg q)

formaNormalNegativa (Disy p q)            = formaNormalNegativa p `Disy` formaNormalNegativa q
formaNormalNegativa (Neg (Disy p q))      = formaNormalNegativa (Neg p `Conj` Neg q)

formaNormalNegativa (Impl p q)            = formaNormalNegativa (Neg p `Disy` q)
formaNormalNegativa (Neg (Impl p q))      = formaNormalNegativa (p `Conj` Neg q)

formaNormalNegativa (Syss p q)            = let a = p `Conj` q
                                                b = Neg p `Conj` Neg q
                                            in formaNormalNegativa $ a `Disy` b
formaNormalNegativa (Neg (Syss p q))      = let a = p `Disy` q
                                                b = Neg p `Disy` Neg q
                                            in formaNormalNegativa $ a `Conj` b

-- Funcion que dada una formula f, regresa la forma normal conjuntiva.
formaNormalConjuntiva :: Prop -> Prop
formaNormalConjuntiva = formaNormalConjuntiva' . formaNormalNegativa
  where
    formaNormalConjuntiva' :: Prop -> Prop
    formaNormalConjuntiva' (Conj p q) = formaNormalConjuntiva' p `Conj` formaNormalConjuntiva' q
    formaNormalConjuntiva' (Disy p q) = formaNormalConjuntiva' p `dist` formaNormalConjuntiva' q
    formaNormalConjuntiva' p          = p
    
    dist :: Prop -> Prop -> Prop
    dist (Conj p1 p2) q = (p1 `dist` q) `Conj` (p2 `dist` q)
    dist p (Conj q1 q2) = (p `dist` q1) `Conj` (p `dist` q2)
    dist p q            = p `Disy` q

-- Funcion que dada una formula f, regresa la forma normal disyuntiva.
formaNormalDisyuntiva :: Prop -> Prop
formaNormalDisyuntiva = formaNormalDisyuntiva' . formaNormalNegativa
  where
    formaNormalDisyuntiva' :: Prop -> Prop
    formaNormalDisyuntiva' (Conj p q) = formaNormalDisyuntiva' p `dist` formaNormalDisyuntiva' q
    formaNormalDisyuntiva' (Disy p q) = formaNormalDisyuntiva' p `Disy` formaNormalDisyuntiva' q
    formaNormalDisyuntiva' p       = p
    
    dist :: Prop -> Prop -> Prop
    dist (Disy p1 p2) q = (p1 `dist` q) `Disy` (p2 `dist` q)
    dist p (Disy q1 q2) = (p `dist` q1) `Disy` (p `dist` q2)
    dist p q            = p `Conj` q

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
varsrp = ["p"]
form1 = ((p \/ q) /\ (((¬) q) \/ r))
interp1 = interp varsrp form1

-- Prueba elimaEquiv
form2 = (p <--> q)
form3 = (q <--> (form2))
elimEquiv1 = elimEquiv form2
elimEquiv2 = elimEquiv form3 

-- Prueba elimImpl
form4 = (p --> (q --> r))
elimImpl1 = elimImpl form4 -- p --> (q --> r) => ¬p v (q --> r) => ¬p v (¬q v r)

-- Prueba esTautologia 
taut1 = (p \/ (¬) p)
taut2 = ((p \/ q) \/ ((¬)p /\ q))
pruebaTaut1 = esTautologia taut1
pruebaTaut2 = esTautologia taut2

-- Prueba esContradiccion
cont1 = ((p \/ q) /\ ((¬)p /\ (¬) q))
pruenbaCont1 = esContradiccion cont1

potencia1 = potencia [1,2,3]

-- Prueba negFormaNormalNegativa
form5 = (p --> (q --> r))
form6 = (p <--> (q --> r))

-- Prueba formaNormalNegativa
form7 = ((¬)((¬)r /\ ((¬)p \/ q)))

-- Pruena formaNormalConjutiva
form8 = (p \/ (q --> r))
form9 = (q --> r)