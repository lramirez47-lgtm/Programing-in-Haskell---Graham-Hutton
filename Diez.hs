{-
Ejercicio 2.
Using recursion, define a version of putBoard :: Board -> IO () that displays nim boards of
any size, rather than being specific to boards with just five rows of stars. Hint: first define an
auxiliary function that takes the current row number as an additional argument.
-}

-- Definimos el tipo Board como una lista de enteros
type Board = [Int]

-- Función auxiliar para imprimir una sola fila con su número correspondiente
putRow :: Int -> Int -> IO ()
putRow row num = do 
    putStr (show row ++ ": ")
    putStrLn (concat (replicate num "* "))

-- Función principal que toma el tablero y llama a una recursión auxiliar
putBoard :: Board -> IO ()
putBoard b = putBoardAux 1 b

-- Función recursiva que lleva el conteo de en qué fila vamos
putBoardAux :: Int -> Board -> IO ()
putBoardAux _ []     = return ()
putBoardAux r (n:ns) = do 
    putRow r n
    putBoardAux (r + 1) ns


{-
Ejercicio 4.
Define an action adder :: IO () that reads a given number of integers from the keyboard, one
per line, and displays their sum
-}

-- Acción principal que pregunta cuántos números se van a sumar
adder :: IO ()
adder = do 
    putStr "Cuantos numeros?"
    nStr <- getLine
    let n = read nStr :: Int
    total <- sumInputs n 0
    putStrLn ("The total is " ++ show total)

-- Función recursiva que lee n números y acumula la suma
sumInputs :: Int -> Int -> IO Int
sumInputs 0 acc = return acc
sumInputs n acc = do 
    numStr <- getLine
    let num = read numStr :: Int
    sumInputs (n - 1) (acc + num)


{-
Ejercicio 6.
Using getCh, define an action readLine :: IO String that behaves in the same way as
getLine, except that it also permits the delete key to be used to remove characters. Hint: the delete
character is ’\DEL’, and the control character for moving the cursor back one space is ’\b’.
-}

-- Iniciamos la lectura con un acumulador de cadena vacío
readLine :: IO String
readLine = getChars []
getChars :: String -> IO String
getChars xs = do 
    x <- getChar
    case x of
        -- Si presiona Enter, devolvemos la cadena acumulada (invirtiendo el orden)
        '\n'   -> do putChar '\n'
                     return (reverse xs)
        -- Si presiona Backspace, verificamos que haya algo que borrar
        '\DEL' -> if null xs 
                  then getChars xs 
                  else do putStr "\b \b" -- Imprime retroceso, espacio, retroceso visualmente
                          getChars (tail xs)
        -- Si es cualquier otro carácter, lo agregamos a la cabeza del acumulador
        _      -> getChars (x:xs)