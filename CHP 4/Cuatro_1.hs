{-
  Ejercicio 1.
  Using library functions, define a function halve :: [a] -> ([a],[a]) that splits an even lengthed list into two halves. For example:
  > halve [1,2,3,4,5,6] 
  ([1,2,3],[4,5,6])
-}

-- Para resolver esto, necesitamos definir donde cortar. 
-- El corte justo a la mitad lo obtenemos dividiendo la longitud total de la lista entre 2.
-- Usamos 'div' porque necesitamos una división entera.
-- Luego, usamos la función de biblioteca 'splitAt' que toma un índice y una lista, 
-- y devuelve una tupla con las dos partes cortadas exactamente en ese índice.

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs