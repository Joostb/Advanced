implementation module cashModel
/*
	Pieter Koopman, Radboud University, 2016
	pieter@cs.ru.nl
	Advanced programming
	
	A simple state model for an automated cash register
*/

import StdEnv, StdGeneric, GenEq, gastjeStart

f :: Euro -> Euro
f a = a

Start = test (\a c .(f a)-.c)

derive string Euro
derive gen Euro

class euro a :: a -> Euro
instance euro Product where
	euro Pizza = euro (4,99)
	euro Beer  = euro (0,65)
	euro _     = euro 1

instance euro Int where euro e = {euro = e, cent = 0}
instance euro (Int, Int) where euro (e,c) = {euro = e, cent = c}
instance euro [e] | euro e where euro l = sum (map euro l)
instance euro Euro where euro e = e
instance + Euro where
	+ x y = {euro = c / 100, cent = (abs c) rem 100} where
		c = (x.euro + y.euro) * 100 + sign x.euro * x.cent + sign y.euro * y.cent
instance - Euro where
	- x y = {euro = c / 100, cent = (abs c) rem 100} where
		c = (x.euro - y.euro) * 100 + sign x.euro * x.cent - sign y.euro * y.cent
instance zero Euro where zero = {euro = 0, cent = 0}
derive gEq Euro,  Product
instance == Product where (==) p q = p === q
instance == Euro where (==) p q = p === q

instance ~ Euro where ~ e = {e & euro = ~e.euro}

model :: [Product] Action -> ([Product],[Euro])
model list (Add p) = ([p:list],[euro p])
model list (Rem p) | isMember p list
 = (removeMember p list,[~ (euro p)])
 = (list,[])
model list Pay = ([],[euro list])

//Properties 1
:: Minus a = (-.) a a

instance prop (Minus Euro) where 
	holds (a -. b) p = [{info=[" ", string{|*|} a,"-", string{|*|} b :p.info], bool = ((a-b) == euro (a.euro - b.euro - if (a.cent - b.cent < 0) (1) (0), if (a.cent - b.cent < 0) (100 - a.cent - b.cent) (a.cent - b.cent)))}]
	
:: Plus a = (+.) a a

instance prop (Plus Euro) where 
	holds (a +. b) p = [{info=[" ", string{|*|} a,"+", string{|*|} b :p.info], bool = (a+b) == euro (a.euro + b.euro + ((a.cent + b.cent) / 100), (a.cent + b.cent) rem 100)}]
	
:: Neg a = ~. a

instance prop (Neg Euro) where 
	holds (~.a) p = [{info=[" ", string{|*|} a,"~" :p.info], bool = ~a == euro 0 - a}]
	
	



