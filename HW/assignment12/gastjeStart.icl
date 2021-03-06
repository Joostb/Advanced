implementation module gastjeStart

/*
	Pieter Koopman, Radboud University, 2016
	pieter@cs.ru.nl
	Advanced programming
	A simplified MBT tool based on logical properties
	
	Use the iTask environment!
*/

import StdEnv, StdGeneric, GenEq

pUpper :: Char -> Bool
pUpper c = c <> toUpper c

:: FORA a b = (For) (a -> b) [a] 

instance prop (FORA a b) | testArg a & prop b  where
	holds (f For l) p = diagonal [holds (f a) {p & info = [" ",string{|*|} a:p.info]} \\ a <- l]

:: Implies a b = (==>) a  b 

instance prop (Implies Bool Bool)  where
	holds (a ==> b) p | a && not b 	= [{bool=False, info=[" ",string{|*|} a, "a is niet waar":p.info]}]
							  		=  [{p & info = [string{|*|} b,"implies is gelukt": p.info]}]
							  		
:: AEQB a = (=.=) a a 

instance prop (AEQB a) | testArg a where
	holds (a =.= b) p | (gEq{|*|} a b) 	= [{p & info = [string{|*|} b,"vergelijking is gelukt": p.info]}]
							  		= [{bool=False, info=["a <> b: %i%i ",string{|*|} a, string{|*|} b, "a is niet b, helaas":p.info]}]

saai :: Int Int -> AEQB Int
saai a b = a =.= b

Start = ["pUpper: ":test (pUpper For ['a'..'z'] )]

test :: p -> [String] | prop p
test p = check 1000 (holds p prop0)

check :: Int [Prop] -> [String]
check n [] = ["Proof\n"]
check 0 l  = ["Passed\n"]
check n [p:x] | p.bool
	= check (n-1) x
	= ["Fail for: ":reverse ["\n":p.info]]

class prop a where holds :: a Prop -> [Prop]

instance prop Bool where holds b p = [{p & bool = b}]

// crashes the iTask compiler if there is no dcl module :(
instance prop (a->b) | prop b & testArg a 
where
	holds f p = diagonal [holds (f a) {p & info = [" ",string{|*|} a:p.info]} \\ a <- gen{|*|}]

class testArg a | gen{|*|}, string{|*|}, gEq{|*|} a 

prop0 = {bool = True, info = []}

generic gen a :: [ a ]
gen{|Int|}  = [0,1,-1,maxint,minint,maxint-1,minint+1:[j\\i<-[2..], j<-[i,~i]]]
gen{|Bool|} = [True,False]
gen{|Char|} = [' '..'~'] ++ ['\t\n\b']
gen{|UNIT|} = [UNIT]
gen{|PAIR|}   f g	= map (\(a,b)=PAIR a b) (diag2 f g)
gen{|EITHER|} f g = merge (map RIGHT g) (map LEFT f)
where
  merge [a:x] ys = [a: merge ys x]
  merge []    ys = ys
gen{|CONS|}   f  = map CONS f
gen{|OBJECT|} f  = map OBJECT f
gen{|RECORD|} f  = map RECORD f
gen{|FIELD|}  f  = map FIELD f

generic string a :: a -> String
string{|Int|} i = toString i
string{|Bool|} b = toString b
string{|Char|} c = toString ['\'',c,'\'']
string{|UNIT|} _ = ""
string{|PAIR|} f g (PAIR x y) = f x + " " + g y
string{|EITHER|} f g (LEFT x) = f x
string{|EITHER|} f g (RIGHT y) = g y
string{|CONS of gcd|} f (CONS x) | gcd.gcd_arity > 0
	= "(" + gcd.gcd_name + " " + f x + ")"
	= gcd.gcd_name
string{|OBJECT|} f (OBJECT x) = f x
string{|RECORD of grd|} f (RECORD x) = "{" + grd.grd_name + "|" + f x + "}"
string{|FIELD of gfd|} f (FIELD x) = gfd.gfd_name + " = " + f x + " "

maxint :: Int
maxint =: IF_INT_64_OR_32 (2^63-1) (2^31-1) //2147483647

minint :: Int
minint =: IF_INT_64_OR_32 (2^63) (2^31) //-2147483648

instance + String where + s t = s +++ t

diagonal :: [[a]] -> [a]
diagonal list = f 1 2 list []
where
	f n m [] [] = []
	f 0 m xs ys = f m (m+1) (rev ys xs) []
	f n m [] ys = f m (m+1) (rev ys []) []
	f n m [[x:r]:xs] ys = [x: f (n-1) m xs [r:ys]]
	f n m [[]:xs] ys = f (n-1) m xs ys
	
	rev []    accu = accu
	rev [x:r] accu = rev r [x:accu]

