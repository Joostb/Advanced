module assignment9

import StdCleanTypes, StdMisc, StdList, StdInt, Data.Tuple, StdClass, iTasks._Framework.Generic, Text.JSON, Data.Functor, Control.Applicative, Control.Monad, Data.Void
import qualified iTasks
import qualified Text
from Text import class Text, instance Text String
from StdFunc import o
from StdTuple import fst
from Data.Map import :: Map, Bin, Tip, put, get, newMap
from Data.List import union, removeMember, instance Functor []
import qualified Data.List as List

:: Ident	:== String
:: Element  :== Sem Int
:: Set      :== Sem [Int]
:: Variable :== String
:: Val 		= I Int | S [Int]
:: State	:== Map Ident Val
:: Sem a 	= Sem (State -> (a, State))

emptyState :: State
emptyState = newMap

integer :: Int -> Element
integer i = pure i

new :: Set
new = pure []

toList :: Int -> [Int]
toList i = [i]

insert :: Element Set -> Set
insert e s = pure (++) <*> s <*> (fmap toList e)

union :: Set Set -> Set
union s1 s2 = pure 'List'.union <*> s1 <*> s2

intersect :: Set Set -> Set
intersect s1 s2 = pure 'List'.intersect <*> s1 <*> s2

difference :: Set Set -> Set
difference s1 s2 = pure 'List'.difference <*> s1 <*> s2



class variable a :: Variable -> (Sem a)

instance variable Int where
	variable str = Sem (\s . (case (get str s) of (Just (I i)) = i, s))
	
instance variable [Int] where
	variable str = Sem (\s . (case (get str s) of (Just (S is)) = is, s))

size    :: Set -> Element
size s = fmap length s

unS :: (Sem a) -> State -> (a, State)
unS (Sem f) = f

instance Functor (Sem) where
	fmap f (Sem s) = Sem (\x . case (s x) of (a, s2) = (f a, s2))
	
instance Applicative (Sem) where
	pure a = Sem \s . (a, s)
	(<*>) (Sem f) (Sem x) = Sem(\s . case f s of
								(f1, xs) = case x xs of
											(x1, xss) = (f1 x1, xss)
						)
						
instance Monad (Sem) where
	bind (Sem a) f = Sem (\s . case a s of (as, xs) = unS (f as) xs)

instance + Element where
	(+) x y = pure (+) <*> x <*> y
	
instance - Element where
	(-) x y = pure (-) <*> x <*> y
	
instance * Element where
	(*) x y = pure (*) <*> x <*> y
	
instance + Set where
	(+) x y = pure ('List'.union) <*> x <*> y
	
instance - Set where
	(-) x y = pure ('List'.difference) <*> x <*> y
	
eval :: (Sem a) -> (a, State)
eval e = (unS e) emptyState

expr1 :: Element
expr1 = integer 2

expr2 :: Element
expr2 = expr1 + expr1

expr3 :: Element
expr3 = expr1 + expr1 * integer 3

Start = eval (insert expr1 (insert expr3 new))


/*
(:.)  infixl 1 :: (Sem a) (Sem b)             -> Sem b    // sequential composition
(==.) infix  4 :: (Sem a) (Sem a)             -> Sem Bool // equality
(<.)  infix  4 :: (Sem a) (Sem a)             -> Sem Bool // less than
IF    :: (Sem Bool) THEN (Sem a) ELSE (Sem a) -> Sem a    // conditional expression
WHILE :: (Sem Bool) DO (Sem a)                -> Sem ()   // repetition

:: THEN = THEN
:: ELSE = ELSE
:: DO   = DO

// examples

expr1 :: Element
expr1 = integer 2

expr2 :: Element
expr2 = expr1 + expr1

expr3 :: Element
expr3 = expr1 + expr1 * integer 3

expr4 :: Set
expr4 = insert expr3 new

expr5 :: Set
expr5 =
    x =. expr4 :.
    variable x

expr6 :: Element
expr6 =
    x =. insert (integer 11) new :.
    x =. size (variable x) :.
    variable x

expr7 :: Set
expr7 =
    x =. insert (integer 11) new :.
    y =. variable x

expr8 :: Set
expr8 =
    x =. insert (integer 11) new :.
    x =. insert (size (variable x)) (variable x) :.
    variable x

expr9 :: Set
expr9 =
    x =. insert (integer 0) new :.
    IF (size (variable x) ==. integer 0) THEN
        (x =. insert (integer 0) (variable x))
    ELSE
        (x =. delete (integer 0) (variable x)) :.
    variable x

expr10 :: Set
expr10 =
	z =. integer 7 :.
	x =. new :.
	x =. insert (variable z) (variable x) :.
	y =. op (variable x) (variable x) :.
	WHILE (size (variable x) <. integer 5) DO
		(x =. insert (size (variable x)) (variable x)) :.
	z =. op (variable x) -. (intersection (variable x) (insert (variable z) new))

exprTypeError :: Set
exprTypeError =
    x =. integer 0 :.
    variable x

x = "x"
y = "y"
z = "z"
*/

