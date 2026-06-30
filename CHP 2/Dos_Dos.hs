{- 2. Work through the examples from this chapter using GHCi. 
Then, take the following incorrectly formatted script and correct the things 
that are wrong with it:

N = a 'div' length xs
    where
       a = 10
      xs = [1,2,3,4,5] 

-}

{-El código esta mal identado (Haskell se enoja mucho si fallamos en esto), 
las funciones no pueden  empezar con mayúsculas y se deben usar 
comillas invertidas `` y no comillas simples ´´ el código corregido queda:-} 

n = a `div` length xs
    where
       a  = 10
       xs = [1,2,3,4,5]