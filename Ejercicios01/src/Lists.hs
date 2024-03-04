{-
- Lógica Computacional 2024-2
- Profesor: Francisco Hernández Quiroz
- Ayudante: Marco Vladimir Lemus Yáñez
- Ayudante: Naomi Itzel Reyes Granados
- Laboratorio: Emiliano Galeana Araujo
- Laboratorio: José Ricardo Desales Santos
- Practica 1: Listas por compresión 
- Integrantes:
- Rodríguez Dimayuga Laura Itzel  
- Figueroa Barrientos Andrea Valeria 
- Vázquez Franco Rafael Vázquez Franco
-}

module Lists where


--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------

-- Listas por compresion 

-- | power. Funcion que regresa un conjunto potencia 
power :: [a] -> [[a]]
power [] = [[]] -- Caso base
power (x:xs) = [x:s | s <- power xs] ++ power xs -- Incluimos a los subconjuntos que tienen x ++ los que no lo tienen

-- | consecutives. Lista que regresa los numeros de 0 a n 
consecutives :: Int -> [Int]
consecutives n = [0..n]

-- | evenNumbers. Lista que regresa los numeros pares de 0 a n
evenNumbers :: Int -> [Int]
evenNumbers n = [x | x <- [0..n], even x]

-- | multiplesOf. Lista que regresa los multiplos de un numero n hasta k 
multiplesOf :: Int -> Int -> [Int]
multiplesOf n k = [x | x <- [n..k], x `mod` n == 0]

-- | squares. Lista que regresa los cuadrados de los numeros de 1 a n
squares :: Int -> [Int]
squares n = [x^2 | x <- [1..n]]

-- | mapListList. Lista de la multiplicacion de dos listas
mapListList :: [Int] -> [Int] -> [Int]
mapListList xs ys = [x * y | x <- xs, y <- ys]



--------------------------------------------------------------------------------
--------                           AUXILIARES                           --------
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
--------                             PRUEBAS                            --------
--------------------------------------------------------------------------------
-- | testPower. Pruebas para la función power.
testPower :: Bool
testPower = power [1,2,3] == [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

-- | testConsecutives. Pruebas para la función consecutives.
testConsecutives :: Bool
testConsecutives = consecutives 5 == [0,1,2,3,4,5]

-- | testEvenNumbers. Pruebas para la función evenNumbers.
testEvenNumbers :: Bool
testEvenNumbers = evenNumbers 5 == [0,2,4]

-- | testMultiplesOf. Pruebas para la función multiplesOf.
testMultiplesOf :: Bool
testMultiplesOf = multiplesOf 2 10 == [2,4,6,8,10]

-- | testSquares. Pruebas para la función squares.
testSquares :: Bool
testSquares = squares 5 == [1,4,9,16,25]

-- | testMapListList. Pruebas para la función mapListList.
testMapListList :: Bool
testMapListList = mapListList [1,2,3] [4,5,6] == [4,5,6,8,10,12,12,15,18]
