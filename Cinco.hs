import Data.Char -- Necesario para el ejercicio 10 
{-
Ejercicio 1.
Using a list comprehension, give an expression that calculates the sum 1^2 + 2^2 + ... + 100^2 
of the first one hundred integer squares.
-}

-- Simplemente generamos la lista de los cuadrados del 1 al 100 y le aplicamos la función sum.
sumSquares :: Int
sumSquares = sum [x^2 | x <- [1..100]]


{-
Ejercicio 2.
Suppose that a coordinate grid of size m x n is given by the list of all pairs (x, y) 
of integers such that 0 <= x <= m and 0 <= y <= n. Using a list comprehension, define 
a function grid :: Int -> Int -> [(Int, Int)] that returns a coordinate grid of a given size.
-}

-- Generamos pares (x,y) donde x va de 0 a m, y para cada x, y va de 0 a n.
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]


{-
Ejercicio 3.
Using a list comprehension and the function grid above, define a 
function square :: Int -> [(Int, Int)] that returns a coordinate 
grid of size n x n, excluding the diagonal from (0, 0) to (n, n).
-}

-- Reutilizamos la función grid y filtramos los elementos donde x e y sean iguales.
square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]


{-
Ejercicio 4.
In a similar manner to the function length, show how the library function 
replicate :: Int -> a -> [a] that produces a list of identical elements 
can be defined using a list comprehension.
-}

-- Usamos el generador [1..n] para repetir el proceso n veces, pero el valor que metemos en la lista siempre es x.
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]


{-
Ejercicio 5.
A triple (x, y, z) of positive integers is Pythagorean if it satisfies the 
equation x^2 + y^2 = z^2. Using a list comprehension, define a function 
pyths :: Int -> [(Int, Int, Int)] that returns the list of all such triples
whose components are at most a given limit.
-}

-- Generamos todas las combinaciones posibles de x, y, z hasta el límite n y filtramos las que cumplen el teorema de Pitágoras.
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]


{-
Ejercicio 6.
A positive integer is perfect if it equals the sum of all of its factors, 
excluding the number itself. Using a list comprehension and the function factors, 
define a function perfects :: Int -> [Int] that returns the list of all perfect numbers up to a given limit.
-}

-- Primero definimos una función para encontrar los factores de un número.
factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], n `mod` x == 0]

-- Luego, para cada número i hasta el límite n, verificamos si la suma de sus factores es igual a i.
perfects :: Int -> [Int]
perfects n = [i | i <- [1..n], sum (factors i) == i]

{-
Ejercicio 7.
Show how the list comprehension [(x,y) | x <- [1,2], y <- [3,4]] with two 
generators can be re-expressed using two comprehensions with single generators. 
Hint: nest one comprehension within the other and make use of the library function concat :: [[a]] -> [a].
-}

{-Al anidar las comprensiones, la interior genera una lista de tuplas para un x fijo,
y la exterior hace esto para cada valor de x, creando una lista de listas.
Finalmente, concat aplana todo en una sola lista continua.-}

ejercicio7 :: [(Int, Int)]
ejercicio7 = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]


{-
Ejercicio 8.
Redefine the function positions using the function find.
-}

-- Definimos la función find tal como viene en el libro:
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

{- Ahora redefinimos positions. 
zip xs [0..] crea una lista de tuplas donde la clave es el elemento 
y el valor es su índice. Luego usamos 'find' para buscar todos los índices de x.-}
positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])


{-
Ejercicio 9.
The scalar product of two lists of integers xs and ys of length n is given by the sum of the products of corresponding integers.
In a similar manner to chisqr, show how a list comprehension can be used to define a function 
scalarproduct :: [Int] -> [Int] -> Int that returns the scalar product of two lists.
-}

{-Para resolver la sumatoria matemática, usamos zip para emparejar los 
elementos de la misma posición (xs_i y ys_i), los multiplicamos en la 
list comprehension y finalmente sumamos toda la lista resultante.-}
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]


{-
Ejercicio 10.
Modify the Caesar cipher program to also handle upper-case letters.
-}

{-Partiendo del código mostrado en el libro:
Reescribimos la función shift para que evalúe si la letra es minúscula o mayúscula.
Dependiendo del caso, tomamos como base el valor ASCII de 'a' o de 'A'.
Si no es una letra, la dejamos tal cual.-}

shift :: Int -> Char -> Char
shift n c
    | isLower c = chr ((ord c - ord 'a' + n) `mod` 26 + ord 'a')
    | isUpper c = chr ((ord c - ord 'A' + n) `mod` 26 + ord 'A')
    | otherwise = c

-- La función encode se mantiene igual, aplicando nuestro nuevo shift a toda la cadena.
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
