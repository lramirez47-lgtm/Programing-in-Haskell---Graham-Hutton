import Control.Applicative
import System.IO

--Código base del libro

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

instance Functor Parser where
    fmap f p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(f v, out)])

instance Applicative Parser where
    pure v    = P (\inp -> [(v, inp)])
    p1 <*> p2 = P (\inp -> case parse p1 inp of
                            []        -> []
                            [(g,out)] -> parse (fmap g p2) out)

instance Monad Parser where
    p >>= f = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> parse (f v) out)

instance Alternative Parser where
    empty     = P (\inp -> [])
    p <|> q   = P (\inp -> case parse p inp of
                            []        -> parse q inp
                            [(v,out)] -> [(v,out)])

-- Parsers básicos
item :: Parser Char
item = P (\inp -> case inp of
                    []     -> []
                    (x:xs) -> [(x,xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

char :: Char -> Parser Char
char x = sat (== x)

digit :: Parser Char
digit = sat (\x -> x >= '0' && x <= '9')

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

space :: Parser ()
space = do many (sat (\x -> x == ' ' || x == '\n' || x == '\r' || x == '\t'))
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

symbol :: String -> Parser String
symbol xs = token (string xs)
  where string []     = return []
        string (c:cs) = do char c; string cs; return (c:cs)

{-
EJERCICIO 4:
Explain why the final simplification of the grammar for arithmetic expressions has a dramatic
effect on the efficiency of the resulting parser. Hint: begin by considering how an expression
comprising a single number would be parsed if this simplification step had not been made.

El efecto dramático en la eficiencia se debe a que la simplificación elimina el 
retroceso redundante. 
Si no hiciéramos la simplificación, la gramática sería:
    expr ::= term + expr | term

Si el parser intenta evaluar un solo número (ej. "5"):
1. Intenta evaluar la primera rama: `term + expr`.
2. Lee el `term` ("5") con éxito.
3. Busca el símbolo `+`. Falla al no encontrarlo.
4. El parser hace retroceso (backtrack) y pasa a la segunda rama: `term`.
5. Vuelve a leer y procesar el "5" desde cero.

Si en lugar de "5" fuera una expresión matemática gigante entre paréntesis, 
el parser calcularía toda la expresión pesada, fallaría buscando el '+', y tendría 
que recalcularla toda de nuevo. La simplificación evita recalcular el `term` dos veces.
-}

{-
Ejercicio 6.
Extend the parser expr :: Parser Int to support subtraction and division, and to use integer
values rather than natural numbers, based upon the following revisions to the grammar:
-}

expr :: Parser Int
expr = do t <- term
          (do symbol "+"
              e <- expr
              return (t + e)
           <|> do symbol "-"
                  e <- expr
                  return (t - e)
           <|> return t)

term :: Parser Int
term = do f <- factor -- Corregido: Llamamos a factor en lugar de exponentiate
          (do symbol "*"
              t <- term
              return (f * t)
           <|> do symbol "/"
                  t <- term
                  return (f `div` t)
           <|> return f)

-- Definimos factor utilizando 'int' (enteros con signo) en lugar de 'nat' (naturales)
factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> int 

{-
EJERCICIO 8: Consider expressions built up from natural numbers using a subtraction operator that is assumed to
associate to the left.

a. Translate this description directly into a grammar.

expr ::= expr - nat | nat

b. Implement this grammar as a parser expr :: Parser Int.

   expr8_mala = do e <- expr8_mala
                   symbol "-"
                   n <- nat
                   return (e - n)
                <|> nat

c. What is the problem with this parser?

   El problema es la recursión por la izquierda. La primera 
   instrucción de expr8_mala es llamarse a sí misma (`do e <- expr8_mala`) 
   sin haber consumido ningún caracter del texto de entrada. Esto genera un 
   bucle infinito que termina colapsando la memoria.

d. Show how it can be fixed. Hint: rewrite using 'many' and 'foldl'.
-}

expr8 :: Parser Int
expr8 = do n <- nat
           ns <- many (do symbol "-"
                          nat)
           return (foldl (-) n ns)


{-
EJERCICIO 9:
Modify the calculator program to indicate the approximate position of an error rather than just
sounding a beep, by using the fact that the parser returns the unconsumed part of the input string.
-}

-- Función para leer una línea permitiendo usar la tecla Borrar
readLine :: String -> IO String
readLine xs = do
    hSetBuffering stdin NoBuffering 
    c <- getChar
    case c of
        '\n'   -> do putChar '\n'
                     hSetBuffering stdin LineBuffering
                     return (reverse xs)
        '\DEL' -> if null xs 
                  then readLine xs 
                  else do putStr "\b \b" 
                          readLine (tail xs)
        _      -> do putChar c
                     readLine (c:xs)

-- Bucle principal
calculator :: IO ()
calculator = do
    putStr "Calc> "
    hFlush stdout
    input <- readLine ""
    if input == "quit" || input == "exit"
        then putStrLn "Adios!"
        else do
            if null input 
                then calculator
                else case parse expr input of
                    [(result, "")]  -> do putStrLn ("= " ++ show result)
                                          calculator
                    --En lugar de fallar, indicamos cerca de dónde está el texto que no se pudo evaluar
                    [(_, unparsed)] -> do putStrLn ("ERROR: Sintaxis inválida cerca de \"" ++ unparsed ++ "\"")
                                          calculator
                    []              -> do putStrLn "ERROR: Expresión mal estructurada."
                                          calculator



