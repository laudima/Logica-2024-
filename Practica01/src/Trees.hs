{-
- Lógica Computacional 2024-2
- Profesor: Francisco Hernández Quiroz
- Ayudante: Marco Vladimir Lemus Yáñez
- Ayudante: Naomi Itzel Reyes Granados
- Laboratorio: Emiliano Galeana Araujo
- Laboratorio: José Ricardo Desales Santos
- Practica 1: Recordando Haskell. Árboles
- Integrantes:
- Laura Itzel Rodriguez Dimayuga 
- Andrea Valeria Figueroa Barrientos
- Rafael Vázquez Franco
-}

module Trees where

import Data.List

data BTree a = Void | Node a (BTree a) (BTree a) deriving (Show, Eq)

--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------

-- | Regresa el número de nodos de un árbol.
nNodes :: BTree a -> Int
nNodes Void = 0 
nNodes (Node _ left right) = 1 + nNodes left + nNodes right 

-- | Regresa el número de hojas de un árbol.
nLeaves :: BTree a -> Int
nLeaves Void = 0 
nLeaves (Node _ Void Void) = 1 
nLeaves (Node _ left right) = nLeaves left + nLeaves right 

-- | Regresa el número de nodos internos de un árbol.
nni :: BTree a -> Int
nni Void = 0 
nni (Node _ Void Void) = 0 
nni (Node _ left right) = 1 + nni left + nni right 

-- | Nos dice si un elemento está contenido en un árbol ordenado.
contains :: (Ord a, Eq a) => a -> BTree a -> Bool
contains _ Void = False
contains x (Node value left right)
    | x == value = True
    | x < value  = contains x left
    | otherwise  = contains x right

-- | Recorrido inorder.
inorder :: BTree a -> [a]
inorder Void = []
inorder (Node value left right) = inorder left ++ [value] ++ inorder right

-- | Recorrido preorder.
preorder :: BTree a -> [a]
preorder Void = []
preorder (Node value left right) = [value] ++ preorder left ++ preorder right

-- | Recorrido postorder.
postorder :: BTree a -> [a]
postorder Void = []
postorder (Node value left right) = postorder left ++ postorder right ++ [value]

-- | Agrega un elemento a un árbol binario de manera ordenada.
add :: (Ord a) => a -> BTree a -> BTree a
add x Void = Node x Void Void
add x (Node value left right)
    | x == value = Node x left right  -- Ya está
    | x < value  = Node value (add x left) right
    | otherwise  = Node value left (add x right)

-- | Pasa una lista a un árbol binario de forma ordenada.
fromList :: (Ord a) => [a] -> BTree a -> BTree a
fromList [] tree = tree
fromList (x:xs) tree = fromList xs (add x tree)
--------------------------------------------------------------------------------
--------                           AUXILIARES                           --------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--------                             PRUEBAS                            --------
--------------------------------------------------------------------------------

v :: BTree Int
v = Void

test :: BTree Int
test = (Node 10
         (Node 5
           (Node 2
             (Node 1 v v)
             (Node 3 v v))
           (Node 8
             (Node 6 v v)
             (Node 9 v v)))
         (Node 15
           (Node 12
             (Node 11 v v)
             (Node 13 v v))
           (Node 17
             (Node 16 v v)
             (Node 18 v v))))

nn1 = nNodes test
-- Regresa: 15

nLeaves1 = nLeaves test
-- Regresa: 8

nni1 = nni test
-- Regresa: 7

contains1 = contains 0 test
-- Regresa: False

contains2 = contains 13 test
-- Regresa: True

inorder1 = inorder test
-- Regresa: [1,2,3,5,6,8,9,10,11,12,13,15,16,17,18]

preorder1 = preorder test
-- Regresa: [10,5,2,1,3,8,6,9,15,12,11,13,17,16,18]

postorder1 = postorder test
-- Regresa: [1,3,2,6,9,8,5,11,13,12,16,18,17,15,10]

add1 = add 0 test
-- Regresa:
-- Node 10
--   (Node 5
--     (Node 2
--      (Node 1
--       (Node 0 Void Void)
--       Void)
--      (Node 3 Void Void))
--     (Node 8
--       (Node 6 Void Void)
--       (Node 9 Void Void)))
--   (Node 15
--     (Node 12
--      (Node 11 Void Void)
--      (Node 13 Void Void))
--     (Node 17
--       (Node 16 Void Void)
--       (Node 18 Void Void)))

add2 = add 20 test
-- Regresa:
-- Node 10
--   (Node 5
--     (Node 2
--       (Node 1 Void Void)
--       (Node 3 Void Void))
--     (Node 8
--       (Node 6 Void Void)
--       (Node 9 Void Void)))
--   (Node 15
--     (Node 12
--       (Node 11 Void Void)
--       (Node 13 Void Void))
--     (Node 17
--       (Node 16 Void Void)
--       (Node 18 Void
--         (Node 20 Void Void))))

add3 = add 7 v
-- Regresa: Node 7 Void Void

fromList1 = fromList [5,3,8,1,2,6,9,7,10,4] Void
-- Regresa:
-- Node 5
--   (Node 3
--     (Node 1 Void
--       (Node 2 Void Void))
--     (Node 4 Void Void))
--   (Node 8
--     (Node 6 Void
--       (Node 7 Void Void))
--     (Node 9 Void
--       (Node 10 Void Void)))
