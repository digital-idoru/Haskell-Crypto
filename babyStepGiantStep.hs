
--Daniel Hono 2014
-- Implementation of the Baby Step Giant Step algorithm
-- Used to "solve" discrete log problems 
-- Given g and b, find x s.th g^x = b (mod p), for some prime p. 
-- Pretty sure g has to be a generator of Z_p
-- Apparently g has to be an element of known order. 

--Need the Russian Peasant implementation for exponentiation
import Russian_Peasant

--Instead of a hashtable, we're gonna try to use a map! 
import qualified Data.Map.Lazy as Map

--Function to compute the discrete log 
--Input: a prime p, a beta,  and a generator of Z_p units, i.e ord(g) = p-1 
-- Finds x s.th g^x = b \in Z_p units
--babyStepGiantStep :: Integral a => a -> a -> a -> a  
--babyStepGiantStep p g b 
            

--Function to help compute the value m 
computeM :: Integer -> Integer
computeM = ceiling . sqrt . fromIntegral --DAT SEXY ETA-REDUCTION. \x.(f x) <-> f 


--Functions to create the babylist

--Wrapper for creating the babylist 
computeBList :: Integer -> Integer -> [(Integer, Integer)]
computeBList g p = bList g p 0 (computeM  p-1)  [(1,0)]


-- g is the generator, p is the prime, and n needs to be started at 0 
bList :: Integral a => a -> a -> a -> a -> [(a,a)] -> [(a,a)]
bList g p n m (x:xs) 
      | n == m-1 = reverse (x:xs) -- Need to reverse it since it builds up the list by appending at the head. 
      | otherwise = bList g p (n+1) m (((rP (n+1) g p),(n+1)):(x:xs))

               








