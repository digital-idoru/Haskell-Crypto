-- Function(s) to implement the MIller-Rabin primality test. Check if a large number is prime. 
-- Daniel Hono 2014/3/8 


--Miller-Rabin sequence fails when composite number is found. 
-- Codifiy this by using Just and Nothing. 
import Data.Maybe 

-- Need Russian Peasant and some List operations. 
import Data.List
import Russian_Peasant 


--Not sure how useful it is to have a datatype for Primality, but it's kinda cool. 
data Primality = FAIL | PROBABLYPRIME 
	deriving(Show, Eq)

--Kind of messy, but it outputs the probability that n is a prime number after conducting r tests with initial seed a. 
millerTest n r a c
	| result == Nothing = Nothing -- Test failed 
	| r == 0 = Just (1 - (1/4)^c) --We finished r tests without failure, output p("n is prime")
	| otherwise = millerTest n (r-1) (f a `mod` n) (c+1) --Continue the test on the next value
 		where
			f = (\x -> x^2 + 1)
			result = miller n a 




-- Function to implement the Miller-Rabin test. 
-- The function returns Just True if "probably prime" 
-- Right now an a has to be choosen
-- TODO: Random number generator, return p("Composite") = 1/(4^{r}), run r trials. 	
miller :: Integral a => a -> a -> Maybe Primality  
miller n a
	| n <= 2 = Nothing 		-- If we're less than two then fail. 
	| n `mod` 2 == 0 = Nothing 	-- We want an odd prime, i.e. > 2 
	| otherwise = testSequence (createMillerList a n) --Preprocessing is done, build the list and test it. 
		where testSequence (x:xs) 
			| last xs /= 1 = Nothing -- Last element isn't 1 => Fermat Witness => composite, so Fail. 
			| otherwise = fermatSequence (x:xs)  -- We have a "Fermat Sequence" (my own terminology), so continue the test. 
				where 
					fermatSequence [] = Just PROBABLYPRIME  -- Parsed the entire list without anything causing us to fail. 
					fermatSequence (x:xs) 
						| x == 1 = fermatSequence xs --We see a 1, keep going
						| x /= 1 && head xs == 1 = if x /= n-1 then Nothing else fermatSequence xs  -- nontrivial sqrt(1) => composite
						| otherwise = fermatSequence xs 
			

--Create the list of powers. 
-- By using Haskell's laziness, we can unfold a list by squaring the seed mod n and only evaluate it k+1 times. 
createMillerList :: Integral a => a -> a -> [a]
createMillerList a n = mrl a n (highestTwo (n-1) (0,1))
	where mrl a n (k,m) = take (k+1)  $ unfoldr (\y -> Just (y, y^2 `mod` n)) (rP m a n)


--Find the highest power of two that divides n-1. 
--Returns a tuple, k, m s.th n-1 = (2^k)m 
highestTwo :: (Integral t1, Num t) => t1 -> (t, t1) -> (t, t1)
highestTwo n (k, m)
	| n `mod` 2 == 0 = highestTwo (quot n 2)  (k+1, (quot n 2))
	| otherwise = (k, m)

