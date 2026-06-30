{-
Ejercicio 1.
In a similar manner to the function add, define a recursive multiplication 
function mult :: Nat -> Nat -> Nat for the recursive type of natural numbers.
-}

-- Definición del libro del tipo Nat (Números de Peano) y de la función add.

data Nat = Zero | Succ Nat
           deriving (Show, Eq)

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

{-Solución:
Multiplicar por Zero siempre da Zero.
Multiplicar el sucesor de m por n equivale a sumar n al resultado de (m * n).-}

mult :: Nat -> Nat -> Nat
mult Zero     _ = Zero
mult (Succ m) n = add n (mult m n)


{-
Ejercicio 3.
Consider the following type of binary trees:
data Tree a = Leaf a | Node (Tree a) (Tree a)
Define a function balanced :: Tree a -> Bool that decides if a tree is balanced.
-}

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving (Show, Eq)

-- Función auxiliar para contar cuántas hojas tiene un árbol.
leaves :: Tree a -> Int
leaves (Leaf _)   = 1
leaves (Node l r) = leaves l + leaves r

{-Solución:
Un árbol está balanceado si es una hoja, o si ambos subárboles 
están balanceados y la diferencia de hojas entre ellos es a lo mucho 1. -}

balanced :: Tree a -> Bool
balanced (Leaf _)   = True
balanced (Node l r) = abs (leaves l - leaves r) <= 1 
                      && balanced l 
                      && balanced r


{-
Ejercicio 5.
Given the type declaration: data Expr = Val Int | Add Expr Expr
define a higher-order function folde that replaces each Val constructor 
in an expression by a given function, and each Add by another given function.
Then use it to define eval and size.
-}

data Expr = Val Int | Add Expr Expr
            deriving (Show, Eq)

{-Solución:
folde recorre el árbol de expresiones aplicando la función 'f' a los 
valores (hojas) y 'g' a las sumas (nodos internos).-}
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val n)   = f n
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- 'eval' evalúa la expresión: los valores se quedan igual (id), 
-- y las sumas se suman con el operador nativo (+).
eval :: Expr -> Int
eval = folde id (+)

-- size cuenta los valores: cada valor cuenta como 1 (usamos const 1 para 
-- ignorar el número real), y luego se suman las ramas (+).
size :: Expr -> Int
size = folde (const 1) (+)


{-
Ejercicio 7.
Complete the following instance declarations to make Maybe and lists 
instances of the Eq class.
-}

{- 
instance Eq a => Eq (Maybe a) where
    Just x  == Just y  = x == y
    Nothing == Nothing = True
    _       == _       = False

instance Eq a => Eq [a] where
    []     == []     = True
    (x:xs) == (y:ys) = x == y && xs == ys
    _      == _      = False
-}


{-
Ejercicio 9.
Extend the abstract machine to support the use of multiplication.
-}

data ExprM = ValM Int | AddM ExprM ExprM | MultM ExprM ExprM
             deriving (Show, Eq)

-- Extendemos el tipo de Operaciones de control (Cont).
data Op = EVAL_ADD ExprM | ADD_OP Int | EVAL_MULT ExprM | MULT_OP Int
type Cont = [Op]

-- evalM evalúa la expresión guardando la operación pendiente en el control.

evalM :: ExprM -> Cont -> Int
evalM (ValM n)    c = exec c n
evalM (AddM x y)  c = evalM x (EVAL_ADD y : c)
evalM (MultM x y) c = evalM x (EVAL_MULT y : c)

-- exec ejecuta la pila de control sobre el valor acumulado.
exec :: Cont -> Int -> Int
exec []                n = n
exec (EVAL_ADD y : c)  n = evalM y (ADD_OP n : c)
exec (ADD_OP n : c)    m = exec c (n + m)
exec (EVAL_MULT y : c) n = evalM y (MULT_OP n : c)
exec (MULT_OP n : c)   m = exec c (n * m)

-- Función final que inicia la máquina abstracta.
value :: ExprM -> Int
value e = evalM e []