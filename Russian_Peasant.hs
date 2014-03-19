module Russian_Peasant (rP) where



--Compute modular exponentiation using the Russian peasant algorithm
-- y^x % n 
rP :: Integral a => a -> a -> a -> a
rP x y n = myRussianPeasant x y 1 n 
	where myRussianPeasant x y s n
	 	| x <= 0 = s
		| x `mod` 2 == 1 = myRussianPeasant (quot x 2) ((y * y) `mod` n) ((s * y) `mod` n) n
		| otherwise = myRussianPeasant (quot x 2) (y*y `mod` n) s n

