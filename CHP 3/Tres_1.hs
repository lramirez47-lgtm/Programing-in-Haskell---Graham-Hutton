{-
  Ejercicio 1.
  What are the types of the following values?

  a) ['a','b','c']
  b) ('a','b','c')
  c) [(False, 'O'), (True, '1')]
  d) ([False, True], ['0', '1'])
  e) [tail, init, reverse]

-}

-- a) Es una lista donde todos los elementos son caracteres (comillas simples).
-- Tipo: [Char]  
-- b) Es una tupla de tres posiciones, cada una contiene un carácter.
-- Tipo: (Char, Char, Char)

-- c) Es una lista cuyos elementos son tuplas. Cada tupla tiene un Booleano y un carácter.
-- Tipo: [(Bool, Char)]

-- d) Es una sola tupla con dos elementos: el primero es una lista de Booleanos 
-- y el segundo es una lista de caracteres.
-- Tipo: ([Bool], [Char])

-- e) Es una lista que contiene funciones. Todas estas funciones (tail, init, reverse)
-- toman una lista de cualquier tipo 'a' y devuelven una lista del mismo tipo 'a'.
-- Tipo: [[a] -> [a]]