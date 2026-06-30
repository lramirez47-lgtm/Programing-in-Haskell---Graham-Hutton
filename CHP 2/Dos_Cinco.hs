{-5. The library function init removes the last element from a non-empty list;
for example, init [1,2,3,4,5] = [1,2,3,4]. Show how init could be defined 
in two different ways. -}

{-1. Podemos usar take (que toma los primeros n elementos de una lista) y si
le restamos 1 nos quedaríamos con todos los elementos menos el ultimo: -}

mInit1 :: [a] -> [a]
mInit1 xs = take (length xs - 1) xs

{-2. Podemos usar la función tail (que sabemos que quita el primer elemento
de una lista), pero si invertimos la lista usando reverse, ese primer elemento
es en realidad el último y solo quedaría volver a voltear la lista: -}

mInit2 :: [a] -> [a]
mInit2 xs = reverse (tail (reverse xs))