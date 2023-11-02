--Codigo incremento 
-- 1       12%
-- 2        9%
-- 3        5%

funcion1 codigo monto | codigo == 1 = monto * (1 + 0.12)
                      | codigo == 2 = monto * (1 + 0.09)
                      | codigo == 3 = monto * (1 + 0.05)
                      | otherwise = 0

funcion2 lista desde = [x | x <- lista, x > desde, even x]

funcion3 [] _ = 0
funcion3 (x:xs) y = if x > y then 1 + funcion3 xs y else funcion3 xs y