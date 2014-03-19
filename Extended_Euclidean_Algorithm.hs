-- Function to compute Bezout Coeffecients using the extended Euclidean Algorithm. 
-- Returns a tuple that contains (x,y) s.th ax + by = gcd(a,b)


module Extended_Euclidean_Algorithm (exGcd) where 

-- Needs to be called on x, y, r = 1, u1 = 1, v1 = 0, u2 = 0, v2 = 1
--Compute Bezout coeffecients using the extended Euclidean Algorithm
--Returns a tuple of the solution.
exGcd :: Integral a => a -> a -> a -> a -> a -> a -> a -> (a, a)
exGcd x y r u1 v1 u2 v2 
	| y <= 0 = (u1, v1)
	| otherwise = exGcd y (x `mod` y) (x `mod` y) u2 v2 (u1 - (u2*q)) (v1 - (v2*q)) 
		where q = quot x y
