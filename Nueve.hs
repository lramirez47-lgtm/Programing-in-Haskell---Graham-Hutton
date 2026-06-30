{-
Ejercicio 2.
Define a recursive function isChoice :: Eq a => [a] -> [a] -> Bool 
that decides if one list is chosen from another, without using the 
combinatorial functions perms and subs.
-}

-- Primero creamos una función auxiliar que elimina solo la primera  aparición de un elemento en una lista.

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst x (y:ys)
    | x == y    = ys
    | otherwise = y : removeFirst x ys

{- isChoice verifica si todos los elementos de la primera lista están en la segunda.
Si encontramos un elemento, lo eliminamos de la segunda lista para no 
contarlo dos veces, y seguimos recursivamente.-}
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x:xs) ys = elem x ys && isChoice xs (removeFirst x ys)


{-
Ejercicio 4.
Verify that there are 33,665,406 possible expressions over the numbers 
1, 3, 7, 10, 25, 50, and that only 4,672,540 of these expressions 
evaluate successfully.
-}

--Comprobando:

-- Cantidad total de expresiones posibles:
-- length [e | ns <- choices [1,3,7,10,25,50], e <- exprs ns]
-- Resultado: 33665406

-- Cantidad de expresiones válidas (que no se dividen por cero ni dan negativos):
-- length [e | ns <- choices [1,3,7,10,25,50], e <- exprs ns, not (null (eval e))]
-- Resultado: 4672540


{-
Ejercicio 6.
Modify the final program to allow the use of exponentiation.
-}

--Partiendo del código del libro tenemos que: 

-- 1. Agregamos Exp al tipo de datos de Operadores
data Op = Add | Sub | Mul | Div | Exp
          deriving (Show, Eq)

-- 2. Para exponenciación, evitamos exponentes negativos para no tener fracciones, 
-- y evitamos que la base sea 0 o 1 para optimizar el árbol de búsqueda.
valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0
valid Exp x y = y >= 0 && x > 1

-- 3. Agregamos el operador de potencia
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y
