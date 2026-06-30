{-
  Ejercicio 2.
  Define a function third :: [a] -> a that returns the third element in a 
  list that contains at least this many elements using:
  a) head and tail;
  b) list indexing !!;
  c) pattern matching.
-}

-- a) Usando 'head' y 'tail':
-- 'tail' quita el primer elemento, el segundo 'tail' quita el nuevo primer elemento (el segundo original).
-- Ahora el que originalmente era el tercer elemento quedó en la primera posición, así que lo sacamos con 'head'.
thirdA :: [a] -> a
thirdA xs = head (tail (tail xs))

-- b) Usando el operador (!!):
-- Sabiendo que las listas empiezan a contar desde 0. 
-- La posición 0 es el primero, la 1 es el segundo, y el índice 2 es el tercer elemento.
thirdB :: [a] -> a
thirdB xs = xs !! 2

-- c) Usando pattern matching:
-- El patrón (_:_:x:_) ignora explícitamente los dos primeros elementos usando el guion bajo '_'.
-- Al tercer elemento le asigna la variable 'x', y no nos importa si hay más elementos después.
thirdC :: [a] -> a
thirdC (_:_:x:_) = x