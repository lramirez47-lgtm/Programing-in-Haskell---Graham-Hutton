{-
  Ejercicio 7.
  The Luhn algorithm is used to check bank card numbers for simple errors 
  such as mistyping a digit...
  Define a function luhnDouble :: Int -> Int that doubles a digit and 
  subtracts 9 if the result is greater than 9.
-}

{- Multiplicamos el dígito por 2. Si el resultado es estrictamente mayor a 9, 
le restamos 9. Usamos guardas  y where para no calcular  la multiplicación
dos veces.-}

luhnDouble :: Int -> Int
luhnDouble n 
    | doble > 9 = doble - 9
    | otherwise = doble
    where doble = n * 2

{- Ejercicio 8.
  Using luhnDouble and the integer remainder function mod, define a function 
  luhn :: Int -> Int -> Int -> Int -> Bool that decides if a four-digit 
  bank card number is valid.
-}

{-El algoritmo de Luhn para 4 dígitos dice que debemos aplicar la regla de 
luhnDouble a las posiciones impares contando desde la derecha.
Luego sumamos todo. 
Si el residuo de la suma dividida entre 10 es cero, la tarjeta es válida.
-}
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = suma `mod` 10 == 0
    where suma = luhnDouble a + b + luhnDouble c + d