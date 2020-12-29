-- Homework 3
-- Elise Merritt
newtype Poly = Poly [Double] deriving (Show, Eq)
coeffs :: [Double] -> Poly
-- Coeffs: function checks if xs (list of doubles) is empty or contains trailing zeros, and creates a Poly out of xs accordingly
coeffs xs = if null xs then Poly [] else (if xs!!((length xs)-1) /=0 then Poly xs else Poly (reverse(dropWhile (<1) (reverse xs))))
at :: Poly -> Double -> Double
-- At: Function evaluates a Poly with a given value d, using formula a*d^0 + b*d^1 + c*d^2 + ... for p=Poly [a,b,c,...]
-- function creates another list [d^0, d^1, d^2, ...], then multiplies each element of this list by corresponding coefficient in [a,b,c,...] to make new list
-- then elements of new list are added together to get output
at (Poly p) d = sum (zipWith (*) p (map (d^) [0..(length p-1)]))
add :: Poly -> Poly -> Poly
-- Add: Function adds two polynomials by first adding the coefficients, then using coeffs function to handle any empty lists or trailing zeros in output
add (Poly p1) (Poly p2) = coeffs (zipWith (+) p1 p2)