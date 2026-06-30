{-
  Ejercicio 2.
  Write definitions that have the following types; it doesn't matter what the definitions actually do as long as they are type correct.

  a) bools :: [Bool]
  b) nums :: [[Int]]
  c) add :: Int -> Int -> Int -> Int
  d) copy :: a -> (a, a)
  e) apply :: (a -> b) -> a -> b
-}

-- a) bools es simplemente una lista que contiene valores booleanos.
bools :: [Bool]
bools = [True, False, True]

-- b) nums es una lista donde cada elemento es, a su vez, una lista de enteros.
nums :: [[Int]]
nums = [[1, 2, 3], [4, 5]]

-- c) add es una función que recibe tres parámetros de tipo Int y devuelve un Int.
add :: Int -> Int -> Int -> Int
add x y z = x + y + z

-- d) copy toma un argumento de cualquier tipo 'a' y lo devuelve duplicado dentro de una tupla.
copy :: a -> (a, a)
copy x = (x, x)

-- e) apply es una función que recibe otra función 'f' (que transforma 'a' en 'b') 
-- y un argumento 'x' (de tipo 'a'), y le aplica la función a ese argumento.
apply :: (a -> b) -> a -> b
apply f x = f x