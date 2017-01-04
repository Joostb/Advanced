definition module gastjeStart

/*
	Pieter Koopman, Radboud University, 2016
	pieter@cs.ru.nl
	Advanced programming
	A simplified MBT tool based on logical properties
	
	Use the iTask environment!
*/

import StdEnv, StdGeneric, GenEq

test :: p -> [String] | prop p
class prop a where holds :: a Prop -> [Prop]

:: Prop =
	{ bool :: Bool
	, info :: [String]
	}

instance prop Bool
instance prop (a->b) | prop b & testArg a
class testArg a | gen{|*|}, string{|*|}, gEq{|*|} a

generic gen a :: [a]
derive gen Int, Bool, Char, UNIT, PAIR, EITHER, CONS, OBJECT, RECORD, FIELD

generic string a :: a -> String
derive string Int, Bool, Char, UNIT, PAIR, EITHER, CONS of gcd, OBJECT, RECORD of grd, FIELD of gfd

