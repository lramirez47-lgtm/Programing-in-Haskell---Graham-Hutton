{-
Ejercicio 1.
How does the recursive version of the factorial function behave if applied to a negative argument, 
such as (-1)? Modify the definition to prohibit negative arguments by adding a guard to the recursive case.
-}

{- Si se aplica a un número negativo como (-1), la función original nunca alcanzará 
el caso base (0) y seguirá restando 1 infinitamente (-2, -3, -4...), causando un 
desbordamiento de memoria.-}

fac :: Int -> Int
fac 0 = 1
fac n
    | n > 0     = n * fac (n - 1)
    | otherwise = 0 -- Evitamos el error devolviendo 0 


{-
Ejercicio 2.
Define a recursive function sumdown :: Int -> Int that returns the sum of the non-negative integers from a given value down to zero.
-}

{-El caso base es 0. Para cualquier otro número 'n', sumamos 'n' al 
resultado de la función evaluada en 'n - 1'.-}

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)


{-
Ejercicio 3.
Define the exponentiation operator ^ for non-negative integers using the same pattern of 
recursion as the multiplication operator *, and show how 2 ^ 3 is evaluated using your definition.
-}

(^*) :: Int -> Int -> Int  -- Para no confundir con ^ ya definido 
m ^* 0 = 1
m ^* n = m * (m ^* (n - 1))

{- Evaluación teórica de 2 ^* 3:
2 ^* 3 
= 2 * (2 ^* 2)
= 2 * (2 * (2 ^* 1))
= 2 * (2 * (2 * (2 ^* 0)))
= 2 * (2 * (2 * 1))
= 8-}


{-
Ejercicio 4.
Define a recursive function euclid :: Int -> Int -> Int that implements Euclid's 
algorithm for calculating the greatest common divisor of two non-negative integers.
-}

{-El algoritmo de Euclides dice que si los dos números son iguales, ese es el MCD.
Si uno es mayor que otro, al mayor se le resta el menor y se repite el proceso.-}

euclid :: Int -> Int -> Int
euclid x y
    | x == y    = x
    | x > y     = euclid (x - y) y
    | otherwise = euclid x (y - x)


{-
Ejercicio 5.
Using the recursive definitions given in this chapter, 
show how length [1,2,3], drop 3 [1,2,3,4,5], and init [1,2,3] are evaluated.
-}

-- a) length [1,2,3]
-- = 1 + length [2,3]
-- = 1 + (1 + length [3])
-- = 1 + (1 + (1 + length []))
-- = 1 + (1 + (1 + 0))
-- = 3

-- b) drop 3 [1,2,3,4,5]
-- = drop 2 [2,3,4,5]
-- = drop 1 [3,4,5]
-- = drop 0 [4,5]
-- = [4,5]

-- c) init [1,2,3]
-- = 1 : init [2,3]
-- = 1 : (2 : init [3])
-- = 1 : (2 : [])
-- = [1,2]

{-
Ejercicio 6.
Without looking at the definitions from the standard prelude, define the 
following library functions using recursion.
-}
--Agregamos ' para señalar las nuevas definiciones 
-- a) Decide if all logical values in a list are True:
and' :: [Bool] -> Bool
and' []     = True
and' (b:bs) = b && and' bs

-- b) Concatenate a list of lists:
concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss) = xs ++ concat' xss

-- c) Produce a list with n identical elements:
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

-- d) Select the nth element of a list:
-- Nota: En Haskell, los índices de las listas empiezan en 0.
nth :: [a] -> Int -> a
nth (x:_)  0 = x
nth (_:xs) n = nth xs (n - 1)

-- e) Decide if a value is an element of a list:
elem' :: Eq a => a -> [a] -> Bool
elem' _ []     = False
elem' y (x:xs) 
    | y == x    = True
    | otherwise = elem' y xs

{-
Ejercicio 7.
Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that 
merges two sorted lists to give a single sorted list.
-}

{-Evaluamos cuál de los dos primeros elementos es menor o igual.
Lo agregamos a la cabeza de la lista y llamamos recursivamente a 'merge'
con el resto.-}

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


{-
Ejercicio 8.
Using merge, define a recursive function msort :: Ord a => [a] -> [a] 
that implements merge sort.
-}

-- Función auxiliar para dividir la lista a la mitad usando splitAt.
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

{-Algoritmo Merge Sort:
Casos base: Las listas vacías o de un solo elemento ya están ordenadas.
Caso recursivo: Dividimos, ordenamos cada mitad y fusionamos con merge.-}

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort left) (msort right)
    where (left, right) = halve xs


{-
Ejercicio 9.
Construct the library functions that:
a) calculate the sum of a list of numbers;
b) take a given number of elements from the start of a list;
c) select the last element of a non-empty list.
-}

-- a) Sumar los elementos de una lista
sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

-- b) Tomar 'n' elementos del inicio de una lista
-- Hay dos casos base: si nos piden 0 elementos, o si la lista se vacía antes.
take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x : take' (n - 1) xs

-- c) Seleccionar el último elemento
-- El caso base es cuando la lista tiene exactamente un elemento.
last' :: [a] -> a
last' [x]    = x
last' (_:xs) = last' xs