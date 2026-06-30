{-
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