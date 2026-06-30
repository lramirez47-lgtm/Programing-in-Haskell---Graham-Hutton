{-
 Ejercicio 6.
  Do the same for the following version, and note how it formalises the 
  meaning of curried functions:
  mult :: Int -> Int -> Int -> Int
  mult x y z = x * y * z
-}

{- Las funciones currificadas son funciones que toman 
un solo argumento y devuelven otra función que espera el siguiente argumento.
Usando expresiones lambda (\), la función original queda como:-}

mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))