{-
  Ejercicio 4.
  Why is it not feasible in general for function types to 
  be instances of the Eq class? When is it feasible? 
  Hint: two functions of the same type are equal if they 
    give equal results for equal arguments.
-}

{- ¿Por qué no es factible en general?
Para que dos funciones sean consideradas "iguales" usando el operador (==), 
tendríamos que evaluarlas con absolutamente todos los 
valores posibles de su  dominio de entrada y comprobar que 
dan el mismo resultado. Si una función toma 
enteros (Int), tendríamos que hacer miles de millones de comprobaciones. Si el 
tipo es infinito, la computadora se quedaría calculando para siempre. Por eso 
Haskell no lo permite por defecto.

¿Cuándo sí es factible?
Es factible únicamente cuando el tipo de dato de entrada (dominio) es finito 
y muy pequeño. Por ejemplo, si las funciones solo aceptan valores de tipo 'Bool', 
la computadora solo tendría que comprobar dos casos (True y False) para determinar 
si ambas funciones se comportan idéntico, lo cual sería rapidísimo.
-}