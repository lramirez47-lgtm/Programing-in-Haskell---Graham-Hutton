{-
Ejercicio 4.
Modify the final program to:
a. let the user decide if they wish to play first or second;
b. allow the length of a winning line to also be changed;
c. generate the game tree once, rather than for each move;
d. reduce the size of game tree using alpha-beta pruning.

-}
--Partiendo de la base del libro: 

import Data.List (transpose)

-- ESTRUCTURAS BASE 

data Player = O | B | X deriving (Eq, Ord, Show)
type Grid = [[Player]]
data Tree a = Node a [Tree a] deriving Show

-- a. Decidir la longitud de la línea ganadora
winLength :: Int
winLength = 3 

-- Tamaño del tablero
size :: Int
size = 3

empty :: Grid
empty = replicate size (replicate size B)

-- Intercambiar jugador
next :: Player -> Player
next O = X
next B = B
next X = O

-- Solución 


-- b. Lógica para permitir cambiar la longitud de la línea ganadora
lineWins :: [Player] -> Player -> Bool
lineWins [] _ = False
lineWins line p 
    | length (take winLength chunk) == winLength && all (== p) chunk = True
    | otherwise = lineWins (tail line) p
    where chunk = take winLength line

wins :: Player -> Grid -> Bool
wins p g = any (\l -> lineWins l p) (rows ++ cols ++ diags)
  where
    rows  = g
    cols  = transpose g
    diags = [diag g, diag (map reverse g)]
    diag g' = [g' !! n !! n | n <- [0 .. size - 1]]

won :: Grid -> Bool
won g = wins O g || wins X g

-- c. Generar el árbol de juego una sola vez (Estructura del Árbol)

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p | won g     = []
          | full g    = []
          | otherwise = [chop size (put g i p) | i <- [0 .. size^2 - 1], valid g i]

full :: Grid -> Bool
full = notElem B . concat

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

put :: Grid -> Int -> Player -> [Player]
put g i p = xs ++ [p] ++ ys
  where (xs, B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

-- d. Poda Alfa-Beta para reducir el tamaño del árbol de decisiones
-- Evaluamos usando cotas Alpha (mínimo) y Beta (máximo)

alphabeta :: Tree Grid -> Player -> Int
alphabeta t p = alphabeta' (-2) 2 t p

alphabeta' :: Int -> Int -> Tree Grid -> Player -> Int
alphabeta' alpha beta (Node g []) p
    | wins O g  = -1
    | wins X g  = 1
    | otherwise = 0
alphabeta' alpha beta (Node g ts) X = maximize alpha beta ts
  where
    maximize a b [] = a
    maximize a b (t:v) 
        | a' >= b   = a'  -- Poda Beta
        | otherwise = maximize a' b v
        where a' = max a (alphabeta' a b t O)
alphabeta' alpha beta (Node g ts) O = minimize alpha beta ts
  where
    minimize a b [] = b
    minimize a b (t:v)
        | a >= b'   = b'  -- Poda Alpha
        | otherwise = minimize a b' v
        where b' = min b (alphabeta' a b t X)

-- Determinar el mejor movimiento usando la poda Alfa-Beta
bestmove :: Tree Grid -> Player -> Grid
bestmove (Node g ts) p 
    | p == X    = head [g' | Node g' ts' <- ts, alphabeta (Node g' ts') O == maxScore]
    | otherwise = head [g' | Node g' ts' <- ts, alphabeta (Node g' ts') X == minScore]
  where
    scores   = [alphabeta t (next p) | t <- ts]
    maxScore = maximum scores
    minScore = minimum scores

-- Función auxiliar para leer números de la consola
getNat :: IO () -> IO Int
getNat prompt = do
    prompt
    xs <- getLine
    if not (null xs) && all (`elem` ['0'..'9']) xs then
        return (read xs)
    else do
        putStrLn "ERROR: Entrada invalida"
        getNat prompt

human :: Grid -> Player -> IO Grid
human g p = do
    putStrLn "Movimiento Humano:"
    let maxMove = size^2 - 1
    i <- getNat (putStr ("Ingrese un numero (0-" ++ show maxMove ++ "): "))
    if valid g i then
        return (chop size (put g i p))
    else do
        putStrLn "ERROR: Movimiento Invalido"
        human g p

-- c. Bucle principal 
play :: Tree Grid -> Player -> Player -> IO ()
play (Node g ts) p humanPlayer
    | wins O g  = putStrLn "¡Gana O!"
    | wins X g  = putStrLn "¡Gana X!"
    | full g    = putStrLn "Empate"
    | p == humanPlayer = do 
        g' <- human g p
        let nextTree = head [t | t@(Node gt _) <- ts, gt == g']
        play nextTree (next p) humanPlayer
    | otherwise = do 
        putStrLn "Pensando..."
        let g' = bestmove (Node g ts) p
        let nextTree = head [t | t@(Node gt _) <- ts, gt == g']
        -- Mostrar el movimiento de la IA
        mapM_ print g'
        play nextTree (next p) humanPlayer

-- a. Permitir al usuario decidir si desea jugar primero o segundo
main :: IO ()
main = do
    putStr "¿Quieres jugar primero? (s/n): "
    choice <- getLine
    let initialTree = gametree empty O -- 'O' siempre es el primer turno en el árbol
    if choice == "y" || choice == "s" || choice == "Y" || choice == "S" then
        -- Inicia el turno 'O', y el Humano juega con 'O'
        play initialTree O O  
    else
        -- Inicia el turno 'O', pero el Humano juega con 'X' 
        play initialTree O X