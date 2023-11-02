alan :: String -> String

alan x = "La mama de alan"
------------------------------------------------------------------------------------------------------------------------------

tuplarNumerosXD :: Integer -> Integer -> (Integer,Integer)

tuplarNumerosXD x y = (x,y)

------------------------------------------------------------------------------------------------------------------------------
sumarCuadrados :: Integer -> Integer -> Integer

sumarCuadrados x y = x^2 + y^2

------------------------------------------------------------------------------------------------------------------------------
elSucesivoDe :: Integer -> Integer

elSucesivoDe x = x+1
------------------------------------------------------------------------------------------------------------------------------
esMultiploDe :: Integer -> Integer -> Bool

esMultiploDe x y = (mod y x) == 0 

------------------------------------------------------------------------------------------------------------------------------
sumaParesOrd :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)

sumaParesOrd (x,y) (z,l) = (z+x, y+l)
------------------------------------------------------------------------------------------------------------------------------

positivo :: Integer -> String

positivo x = if x>0 then "Positivo" else "Negativo"
------------------------------------------------------------------------------------------------------------------------------
sosBobo :: String -> String

sosBobo x = if x =="Alan" then "Sos bobo" else "No sos bobo"
------------------------------------------------------------------------------------------------------------------------------
cualEsElMenor :: Integer -> Integer -> Integer

cualEsElMenor x y = if x>y then y else x
------------------------------------------------------------------------------------------------------------------------------
sosPositivo :: Integer -> Bool

sosPositivo x = if x>0 then True else False
------------------------------------------------------------------------------------------------------------------------------
cualEsElMenorV2 :: Integer -> Integer -> Integer -> Integer

cualEsElMenorV2 x y z = if x<y && x<z then x else if y<z then y else z
------------------------------------------------------------------------------------------------------------------------------
funcionRara :: Integer -> Integer

funcionRara x | x > 0 = 1 | x < 0 = -1 | x == 0 = 0
------------------------------------------------------------------------------------------------------------------------------
funcionRara2 :: Integer -> Integer -> String

funcionRara2 x y | x > y = "El primero es mas grande" | x < y = "El segundo es mas grande" | otherwise = "Son iguales"
------------------------------------------------------------------------------------------------------------------------------
funcionRara3 :: Integer -> String

funcionRara3 x 				 | x^2 + 5*x > 0 = "Mayor que cero" 
							 | x^2 + 5*x < 0 = "Menor que cero" 
							 | otherwise = "CERO"

funcionRara4 :: Integer -> String

funcionRara4 x | z > 0 = "Mayor que cero" 
							 | z < 0 = "Menor que cero" 
							 | otherwise = "CERO"
							 where z = x^2 + 5*x
------------------------------------------------------------------------------------------------------------------------------
-- ejercicio clase estado academico

calcularPromedio x y z = (x+y+z)/3

notaMenorA x y z t = (((x<t) || (y<t)) || (z<t))

--calcularEstadoAcademico :: Integer -> Integer -> Integer -> String

calcularEstadoAcademico x y z = if (notaMenorA x y z 4)  then "Libre" 
															else 
																if (calcularPromedio x y z < 8) || (notaMenorA x y z 7) then "Regular" 
																else "Promocionado"
------------------------------------------------------------------------------------------------------------------------------

factorial n | n == 0 = 1
						| otherwise = n * factorial (n-1)

sumatoria n |n == 0 = 0
						| n < 0 = -1
						| otherwise = n + sumatoria (n-1)

sumatoriaInclusiva n |n == 0 = 0
										| n < 0 = n + sumatoriaInclusiva (n+1)
										| otherwise = n + sumatoria (n-1)

sumatoriaDigital n | n `div` 10 == 0 = n `mod` 10
                   | otherwise = (n `mod` 10) + sumatoriaDigital((n `div` 10))

	

par :: Integer -> Bool
par n = mod n 2 == 0

contarPares :: [Integer] -> Integer

contarPares [] = 0

contarPares (x:xs) | par x = 1 + contarPares xs
				   | otherwise = contarPares xs

existeEnUnaLista :: Integer -> [Integer] -> Bool

existeEnUnaLista n [] = False

existeEnUnaLista n (x:xs) | n == x = True
						  | otherwise = existeEnUnaLista n xs

contarElementos :: [Integer] -> Integer

contarElementos [] = 0

contarElementos (x:xs) = 1 + contarElementos xs

esListaPar :: [Integer] -> Bool

esListaPar [] = True 

esListaPar (x:xs) | par x == True = esListaPar xs
				  | otherwise = False

--comprendidosEnElIntervalo :: Integer -> Integer -> [Integer] -> [Bool]

comprendidosEnElIntervalo a b [] = []

comprendidosEnElIntervalo a b (x:xs) = if (x >= a &&  x <= b) 
										then 
											True : comprendidosEnElIntervalo a b xs 
											
										else
											False : comprendidosEnElIntervalo a b xs

--elevarLista :: Integer -> [Integer] -> [Integer]

elevarLista n [] = []
elevarLista n (x:xs) = x^n : elevarLista n xs


cantidadNumerosPares lista = length([x | x <- lista, even x])


factorialDeLista lista = [factorial x |x <- lista, x >= 1, x <= 6 ]

incrementar1 n = n+1

incrementarEn1 lista = map incrementar1 lista

sumarListas [] [] = []
sumarListas (x:xs) (y:ys) = x + y : sumarListas xs ys

concatenador a b = a ++ b

concatenadorListas [] [] = []
concatenadorListas (x:xs) (y:ys) = x : y : concatenadorListas xs ys

numOrdenados x y = if x <= y then (x,y) else (y,x)

elevarCuadrado (x, y) = (x^2, y^2)  

elevarTuplasCuadrado [] = []
elevarTuplasCuadrado (x:xs) = elevarCuadrado x : elevarTuplasCuadrado xs 


funcion1 cliente monto | cliente == 1 = monto * 0.97
					   | cliente == 2 = monto * 0.95
					   | cliente == 3 = monto * 0.93
					   | otherwise = 0

funcion2 lista desde hasta = [x | x <- lista, x > desde, x < hasta]

esPar [] = []
esPar (x:xs) | even x = x : esPar xs
			 | otherwise = esPar xs

esMayorA [] _ = []
esMayorA (x:xs) p | x > p = x : esMayorA xs p
				  | otherwise = esMayorA xs p

funcion3 lista p = (fromIntegral(contarElementos(esPar (esMayorA lista p)))/ fromIntegral(contarElementos lista)) * 100 

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
