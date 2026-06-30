import Data.Char
{-
Ejercicio 1.
Show how the list comprehension [f x | x <- xs, p x] can be re-expressed 
using the higher-order functions map and filter.
-}

{- Una list comprehension que filtra y luego transforma puede expresarse
combinando filter  y map.-}
ejercicio1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
ejercicio1 f p xs = map f (filter p xs)


{-
Ejercicio 2.
Without looking at the standard prelude, define the higher-order library 
functions all, any, takeWhile and dropWhile.
-}

-- 'all' verifica si TODOS los elementos cumplen una condición.
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p  -- El punto . es composición de funciones.

-- 'any' verifica si AL MENOS UN elemento cumple la condición.
any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

-- 'takeWhile' toma elementos de la lista MIENTRAS se cumpla la condición.
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x       = x : takeWhile' p xs
    | otherwise = []

{-'dropWhile' descarta elementos MIENTRAS se cumpla la condición, 
y devuelve el resto de la lista en cuanto la condición falla.-}
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
    | p x       = dropWhile' p xs
    | otherwise = x:xs


{-
Ejercicio 3.
Redefine map f and filter p using foldr.
-}

{-foldr es una función que "colapsa" una lista desde la derecha.
Aquí la usamos para reconstruir la lista aplicando f a cada paso.-}
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

{-Para filter, evaluamos con un if: si cumple p, lo pegamos al acumulador; 
si no, solo pasamos el acumulador sin cambios.-}

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

{-
Ejercicio 7.
La función de biblioteca 'unfold' encapsula un patrón de recursión simple 
para producir una lista. Redefine 'chop8', 'map f' e 'iterate f' usándola.
-}

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
    | p x       = []
    | otherwise = h x : unfold p h t (t x)

-- a) chop8: Toma de 8 en 8 elementos de una lista hasta que se vacía.
chop8 :: [Int] -> [[Int]]
chop8 = unfold null (take 8) (drop 8)

{- b) map: Aplica una función a cada elemento. 
El predicado es null (si está vacía terminamos),h es aplicarle 
la función a la cabeza, y t es pasar al resto de la lista.-}
mapUnfold :: (a -> b) -> [a] -> [b]
mapUnfold f = unfold null (f . head) tail

{-c) iterate: Aplica una función repetidamente (f x, f (f x), ...).
El predicado es const False porque iterate genera una lista infinita.-}
iterateUnfold :: (a -> a) -> a -> [a]
iterateUnfold f = unfold (const False) id f


{-
  Ejercicios 8 y 9.
  Modificar el programa transmisor de cadenas para detectar errores de 
  transmisión simples usando bits de paridad, y probarlo con un canal defectuoso.
-}

type Bit = Int

-- Funciones base del libro para convertir entre binario y entero
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

--Agregar bit de paridad (1 si la suma es impar, 0 si es par).
addParity :: [Bit] -> [Bit]
addParity bits = bits ++ [sum bits `mod` 2]

--Encode modificado. Ahora genera bloques de 9 bits en lugar de 8.
encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin . ord)

--Cortar de 9 en 9 bits.
chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

--Checar el bit de paridad. Si coincide, devuelve los 8 bits
--originales (init bits). Si no, lanza un error parando la ejecución.
checkParity :: [Bit] -> [Bit]
checkParity bits
    | sum (init bits) `mod` 2 == last bits = init bits
    | otherwise = error "Error de paridad: los datos han sido alterados en la transmision"

--Decode modificado para verificar paridad antes de convertir.
decode :: [Bit] -> String
decode = map (chr . bin2int . checkParity) . chop9

--Canal de transmisión normal
transmit :: String -> String
transmit = decode . channel . encode
  where channel = id

--Canal defectuoso que simula perder el primer bit de la transmisión
faultyTransmit :: String -> String
faultyTransmit = decode . faultyChannel . encode
  where faultyChannel = tail


{-
Ejercicio 10.
Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] that 
alternately applies its two argument functions to successive elements.
-}

{-Al llamar a la recursión, simplemente invertimos el orden de las funciones.
Primero aplicamos f, y en el siguiente ciclo aplicamos g, y así sucesivamente.-}
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ []     = []
altMap f g (x:xs) = f x : altMap g f xs