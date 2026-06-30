{-
Ejercicio 2.
Complete the following instance declaration to make the partially-applied function type (a ->)
into a functor:
instance Functor ((->) a) where
...
Hint: first write down the type of fmap, and then think if you already know a library function that
has this type.
-}

{-
 Justificación:
  
  1. Ley de Identidad: fmap id = id
     fmap id (Func f) 
     = Func (id . f)      -- Por definición de fmap aplicado a nuestra estructura
     = Func f             -- Ya que la composición con id deja a f intacta.
     
  2. Ley de Composición: fmap (g . h) = fmap g . fmap h
     fmap (g . h) (Func f)
     = Func ((g . h) . f) -- Por definición de fmap
     = Func (g . (h . f)) -- Por la propiedad asociativa de la composición (.)
     = fmap g (Func (h . f))
     = fmap g (fmap h (Func f))
     = (fmap g . fmap h) (Func f)
-}

-- Usamos un wrapper para evitar conflictos.
newtype Funcion r a = Func (r -> a)

instance Functor (Funcion r) where
    -- fmap :: (a -> b) -> Funcion r a -> Funcion r b
    fmap g (Func f) = Func (g . f)

{-
Ejercicio 4.
There may be more than one way to make a parameterised type into an applicative functor. For
example, the library Control.Applicative provides an alternative ‘zippy’ instance for lists, in
which the function pure makes an infinite list of copies of its argument, and the operator <*>
applies each argument function to the corresponding argument value at the same position. Complete
the following declarations that implement this idea:
newtype ZipList a = Z [a] deriving Show
instance Functor ZipList where
-- fmap :: (a -> b) -> ZipList a -> ZipList b
fmap g (Z xs) = ...
instance Applicative ZipList where
-- pure :: a -> ZipList a
pure x = ...
-- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
(Z gs) <$> (Z xs) = ...
The ZipList wrapper around the list type is required because each type can only have at most one
instance declaration for a given class.
-}

newtype MiZipList a = Z [a] deriving Show

instance Functor MiZipList where
    -- fmap simplemente aplica la función a cada elemento interno de la lista
    fmap :: (a -> b) -> MiZipList a -> MiZipList b
    fmap f (Z xs) = Z (map f xs)

instance Applicative MiZipList where
    -- pure debe generar una estructura infinita repitiendo el elemento para que pueda acoplarse con listas de cualquier longitud.
    pure x = Z (repeat x)
    
    -- <*> extrae las funciones y los elementos y los empareja uno a uno
    Z fs <*> Z xs = Z [f x | (f, x) <- zip fs xs]


{-
Ejercicio 6.
Define an instance of the Monad class for the type (a ->).
-}

newtype MiReader r a = MiReader (r -> a)

instance Functor (MiReader r) where
    fmap f (MiReader g) = MiReader (\x -> f (g x))

instance Applicative (MiReader r) where
    -- pure inicializa un entorno ignorando la configuración r
    pure x = MiReader (\_ -> x)
    
    -- <*> evalúa la función y el valor compartiendo el mismo entorno r
    MiReader f <*> MiReader x = MiReader (\r -> f r (x r))

instance Monad (MiReader r) where
    -- >>= (bind) pasa el entorno r a la primera función, toma su resultado,
    -- se lo pasa a f para obtener el nuevo Reader, y le vuelve a inyectar r.
    MiReader g >>= f = MiReader (\r -> let MiReader h = f (g r) in h r)