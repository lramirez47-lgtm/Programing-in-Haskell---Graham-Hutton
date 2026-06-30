{-3. The library function last selects the last element of a non-empty list;
 for example, last [1,2,3,4,5] = 5. Show how the function last could be 
 defined in terms of the other library functions 
 introduced in this chapter. -}

 {-En el capítulo vimos la función reverse (invierte una lista) y 
 head (toma el primer elemento), entonces podemos combinarlas para hacer:  -}

last1 :: [a] -> a
last1 xs = head (reverse xs)

-----------------------------------------------------------------------
{-4. Can you think of another possible definition for the function last? -}

{-También es posible usar el operador !! que nos permite acceder a una
posición de una lista, y lenght que nos da la longitud de una lista. De modo 
que con estos podemos encontrar el ultimo elemento de una lista para 
mostrarlo ya que si la lista es 1,2,3,4 la numeración al empezar en 0 hace
que el 4 este en la posición 3, eso es lenght - 1 = 4 - 1 = 3, entonces
podemos hacer:-}

last2 :: [a] -> a
last2 xs = xs !! (length xs - 1)