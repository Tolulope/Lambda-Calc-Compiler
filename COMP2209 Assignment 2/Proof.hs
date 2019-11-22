-- un-applying the definitions of x, y, z
x <*> (y <*> z)    -- Eqxal to the right side


-- Applying Homomorphism law to the 2nd term
(Just f) <*> ( (Just g) <*> (Just x) )

-- Applying Homomorphism law
(Just f) <*> Just (g x)

-- via the definition of the function composition 
-- operator
(Just f (g x))


-- Applying <*> one more time
(Just (f.g) x)


-- Applying the function composition
(Just (f.g) ) <*> (Just x)


-- Appying <*> to the first two terms again
(Just (.) f g) <*> (Just x)


-- Appying <*> to the first two terms
(Just (.) f) <*> (Just g) <*> (Just x)

-- x, y & z can all be either Nothing or (Just something). 
-- From the definition of <*>, we know that if either 
-- argument to <*> is Nothing, the entire expression redxces
-- to Nothing. 
-- Therefore, I am only going to focxs on the Just cases
(Just (.)) <*> (Just f) <*> (Just g) <*> (Just x)


-- applying definition of pure
(Just (.)) <*> x <*> y <*> z


pure (.) <*> x <*> y <*> z