module Polynomial where

import Data.List

type Degree = Int
type Coef   = Int
type Monomial = (Degree, Coef)

type LaurantPolynomial = [Monomial]



(+.) :: LaurantPolynomial->LaurantPolynomial->LaurantPolynomial
(+.) [] [] = []
(+.) l  [] = l
(+.) [] l  = l
(+.) l1@(x1@(d1,c1):ls1) l2@(x2@(d2,c2):ls2) | d1 == d2 && c1+c2==0 = ((+.) ls1 ls2)
                                            | d1 == d2 = (d1,c1+c2):((+.) ls1 ls2)
                                            | d1 <  d2 = x1:((+.) ls1 l2)
                                            | d1 >  d2 = x2:((+.) l1 ls2)

neg :: LaurantPolynomial->LaurantPolynomial
neg [] = []
neg ((d,c):l) = (d,-c):(neg l)

(-.) :: LaurantPolynomial->LaurantPolynomial->LaurantPolynomial
(-.) [] [] = []
(-.) l  [] = l
(-.) [] l  = neg l
(-.) l1@(x1@(d1,c1):ls1) l2@(x2@(d2,c2):ls2) | d1 == d2 && c1-c2==0 = ((-.) ls1 ls2)
                                            | d1 == d2 = (d1,c1-c2):((-.) ls1 ls2)
                                            | d1 <  d2 = x1:((-.) ls1 l2)
                                            | d1 >  d2 = x2:((-.) l1 ls2)

private_atomic_mul :: Monomial -> Monomial -> LaurantPolynomial
private_atomic_mul (d1,c1) (d2,c2) = [(d1+d2,c1*c2)]

(*.) :: LaurantPolynomial -> LaurantPolynomial -> LaurantPolynomial
(*.) l1 l2 = foldl (\l -> \m2 -> l +. (foldl (\l -> \m1 -> l +. private_atomic_mul m1 m2) [] l1)) [] l2


coef_to_string :: Coef -> String
coef_to_string 0 = ""
coef_to_string 1 = "+"
coef_to_string x | x > 0 = "+" ++ show x
                 | x < 0 = show x

deg_to_string :: Degree -> String
deg_to_string 0 = ""
deg_to_string 1 = "A"
deg_to_string x = "A^{"++show x++"}"

m_to_string :: Monomial -> String
m_to_string (0,coef)
              | coef == 1 = "+1"
              | coef > 1  = "+"++show coef
              | otherwise = show coef
m_to_string (1,coef)
              | coef == 1  = "+A"
              | coef == -1 = "-A"
              | coef > 1   = "+"++show coef++"A"
              | otherwise  = show coef++"A"
m_to_string (deg,coef)
              | coef == 1  = "+A^{"++show deg++"}"
              | coef == -1 = "-A^{"++show deg++"}"
              | coef > 1   = "+"++show coef++"A^{"++show deg++"}"
              | otherwise  = show coef++"A^{"++show deg++"}"


clip_plus :: String -> String
clip_plus x = if x!!0 == '+' then tail x else x

lp_to_string :: LaurantPolynomial -> String
lp_to_string [] = "0"
lp_to_string lp = clip_plus $ foldl (\acm -> \monomial ->acm++m_to_string monomial) "" lp
