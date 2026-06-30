{-
  Ejercicio 3.
  What are the types of the following functions?

  a) second xs = head (tail xs)
  b) swap (x,y) = (y,x)
  c) pair x y = (x,y)
  d) double x = x*2
  e) palindrome xs = reverse xs == xs
  f) twice f x = f (f x)
-}

-- a) second toma una lista, le quita el primer elemento (tail) y devuelve 
-- el nuevo primer elemento (head). Es decir, devuelve el segundo elemento.
second :: [a] -> a
second xs = head (tail xs)

-- b) swap toma una tupla de dos elementos  
-- y devuelve una tupla con los elementos invertidos.
swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)

-- c) pair toma dos argumentos por separado 
-- y los agrupa devolviendo una sola tupla.
pair :: a -> b -> (a, b)
pair x y = (x,y)

-- d) double toma un número 'x' y lo multiplica por 2. 
-- Al usar el operador (*), Haskell exige que el tipo pertenezca a la clase Num.
double :: Num a => a -> a
double x = x * 2

-- e) palindrome invierte una lista y verifica si es igual a la original.
-- Al usar (==), los elementos de la lista deben poder compararse.
-- El resultado final es un booleano.
palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

-- f) twice toma una función 'f' y un valor 'x', y aplica 'f' dos veces.
-- Como el resultado de 'f' vuelve a entrar a 'f', la función debe tomar 
-- y devolver exactamente el mismo tipo 'a'.
twice :: (a -> a) -> a -> a
twice f x = f (f x)