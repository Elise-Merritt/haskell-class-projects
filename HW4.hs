-- Homework 4
-- Elise Merritt
newtype Poly = Poly [Double] deriving (Show, Eq)
coeffs :: [Double] -> Poly
-- Coeffs: Function checks if xs (list of doubles) is empty or contains trailing zeros, and creates a Poly out of xs accordingly
coeffs xs = if null xs then Poly [] else (if xs!!((length xs)-1) /=0 then Poly xs else Poly (reverse(dropWhile (<1) (reverse xs))))
at :: Poly -> Double -> Double
-- At: Function evaluates a Poly with a given value d, using formula a*d^0 + b*d^1 + c*d^2 + ... for p=Poly [a,b,c,...]
-- function creates another list [d^0, d^1, d^2, ...], then multiplies each element of this list by corresponding coefficient in [a,b,c,...] to make new list
-- then elements of new list are added together to get output
at (Poly p) d = sum (zipWith (*) p (map (d^) [0..(length p-1)]))
add :: Poly -> Poly -> Poly
-- Add: Function adds two polynomials by first adding the coefficients, then using coeffs function to handle any empty lists or trailing zeros in output
add (Poly p1) (Poly p2) = coeffs (if (length p1 == length p2) then (zipWith (+) p1 p2) else (if (length p1 > length p2) then (zipWith (+) p1 (p2 ++ (repeat 0))) else (zipWith (+) (p1 ++ (repeat 0)) p2)))
neg :: Poly -> Poly
--Neg: Function multiplies a polynomial by -1
neg (Poly p) = coeffs (zipWith (*) p (repeat (-1)))
--Mult1: Function multiplies a polynomial by a specified constant (helper function for mult)
mult1 x (Poly p) = coeffs( map (*x) p )
--Mult2: Function multiplies a polynomial by it's variable (helper function for mult)
mult2 (Poly []) = (Poly [])
mult2 (Poly (p:p1)) = coeffs( (0:(p:p1)) )
mult :: Poly -> Poly -> Poly
--Mult: Function multiplies two polynomials together using formula a*[x,y,z,..]+[b,c,d,...]*[x,y,z,...]=[a,b,c,d,..]*[x,y,z,...] 
--where [a,b,c,d,...] and [x,y,z,...] are both polynomials
mult (Poly []) (Poly p) = (Poly [])
mult (Poly p) (Poly []) = (Poly [])
mult (Poly (p:p1)) (Poly (q:p2)) = Main.add (mult1 p (Poly (q:p2))) (mult2 (mult (Poly (p1)) (Poly (q:p2)))) 
x :: Poly
x=Poly [0,1]

instance Num Poly where
    (+) = add
    negate = neg
    (*) = mult
    fromInteger 0 = Poly []
    fromInteger n = Poly [fromInteger n]
    abs = error "No abs for Poly"
    signum = error "No signum for Poly"

--Part 3
--(1) Poly[Poly[1,1], Poly [-1,1]]
--(2) (bx+a)*(dy+c)