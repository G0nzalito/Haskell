es_vocal car = elem car vocales
            where vocales = ['a','A','e','E','i','I','o','O','u','U']

es_digito dig = elem dig digitos
            where digitos = ['0'..'9']

--funcion_1 :: Character -> Integer -> Integer

funcion_1 cliente monto | cliente == '1' = monto * 1.03 
                        | cliente == '2' = monto * 1.05
                        | cliente == '3' = monto * 1.07
                        | otherwise = 0

funcion_2 lista desde hasta = [x | x <- lista, x > desde, x < hasta]

contarElementos [] = 0
contarElementos (x:xs) = 1 + (contarElementos xs)

contarElementosMayores _ [] = []
contarElementosMayores p (x:xs) | x > p = x : contarElementosMayores p xs
                                | otherwise = contarElementosMayores p xs

contarElementosPares [] = 0
contarElementosPares (x:xs) | x `mod` 2 == 0 = 1 + contarElementosPares xs
                            | otherwise = contarElementosPares xs

funcion_3 lista p = (contarElementosPares(contarElementosMayores p lista)/contarElementos lista) 


primerElemento (monto, meses) = monto
segundoElemento (monto, meses) = meses


funcion1 meses | meses == 6 = 0.108
               | meses == 9 = 0.125
               | meses == 12 = 0.7
               | meses == 15 = 0.5
               | otherwise = 0

funcion2 (monto, meses) = monto * ((1+ funcion1(meses))^meses)


funcion3 lista = map funcion2 lista

funcion4 _ [] = 0
funcion4 valorPlazo ((monto, meses):xs) | monto > valorPlazo = funcion2 (monto, meses) 
                                        | otherwise =  funcion4 valorPlazo xs
funcion5 (_, 0) = []                            
funcion5 (monto, meses) = (monto, interes, capFinal) : funcion5 (capFinal, meses - 1)
                            where 
                            interes = monto * funcion1(meses)
                            capFinal = monto + interes

-- <-------------------------------------------->
	
-- |==|==|==|==|==|==|==|==|==|==|==|==|==|==|==|
-- |											|
-- |-- /==<<===><==//==<<===><==//==<<===><==/  |
-- |											|
-- |-- | |->><|<>|><<-| \/ /\ <~~ ^= ___|___ |  |
-- |											|
-- |-- /==<<===><==//==<<===><==//==<<===><==/  |
-- |											|	
-- |==|==|==|==|==|==|==|==|==|==|==|==|==|==|==|

-- <-------------------------------------------->
