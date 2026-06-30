{-
  Ejercicio 3.
  Consider a function safetail :: [a] -> [a] that behaves in the same way 
  as tail except that it maps the empty list to itself rather than producing 
  an error. Using tail and the function 
  null :: [a] -> Bool that decides if a list is empty or not, define 
  safetail using:
  a) a conditional expression;
  b) guarded equations;
  c) pattern matching.
-}

{- a) Usando una expresión condicional (if-then-else):
Evaluamos si la lista está vacía con la función 'null'. Si es cierto, 
devolvemos una lista vacía []. Si no, usamos 'tail' normalmente.-}
safetailA :: [a] -> [a]
safetailA xs = if null xs then [] else tail xs

-- b) Usando guarded equations:
safetailB :: [a] -> [a]
safetailB xs
    | null xs   = []
    | otherwise = tail xs

{- c) Usandopattern matching:
Definimos explícitamente qué pasa para cada "forma" que pueda tener la lista.
Si entra una lista vacía [], devolvemos [].
Si entra una lista con elementos, separamos el primero  
y el resto 'xs', y devolvemos solo ese resto.-}
safetailC :: [a] -> [a]
safetailC []     = []
safetailC (_:xs) = xs