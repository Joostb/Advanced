module ass8

/*
	Advanved Progrmming 2016, Assignment 8
	Pieter Koopman, pieter@cs.ru.nl
*/

from iTasks import always, hasValue, :: TaskValue(..), :: Task, :: Stability, :: TaskCont(..), :: Action, updateInformation, 
		viewInformation, class descr, instance descr String, :: UpdateOption, :: ViewOption(..), -||-, -||, ||-, startEngine, 
		class Publishable, >>*, class TFunctor, instance TFunctor Task, class TApplicative, instance TApplicative Task, 
		instance Publishable Task, Void
import qualified iTasks
import Data.Tuple, StdClass, StdTuple, StdList, StdMaybe, StdString, iTasks._Framework.Generic, Data.Functor, Control.Applicative, Control.Monad
from StdFunc import o
import qualified Data.List as List
import qualified Data.Map as Map

:: Expression
	= New
	| Insert		Element Set
	| Delete		Element Set
	| Variable		Ident
	| Intersection	Set 	Set
	| Integer		Int
	| Size			Set
	| Oper			Expression Op Expression
	| (=.) infixl 2 Ident Expression

:: Op 		=	+. | -. | *.
:: Set 		:== Expression
:: Element	:== Expression
:: Ident	:== String

derive class iTask Expression
derive class iTask Op

// === State

:: Val = ValI Int | ValS [Int] | Fail String
:: State :== (Map String Val)

derive class iTask Val

// === semantics
:: Sem a = S (State -> (a, State))

derive class iTask Sem

unS :: (Sem a) -> State -> (a, State)
unS (S f) = f

instance Functor (Sem) where
	fmap f (S s) = S (\x . case (s x) of (a, s2) = (f a, s2))
	
instance Applicative (Sem) where
	pure a = S \s . (a, s)
	(<*>) (S f) (S x) = S(\s . case f s of
								(f1, xs) = case x xs of
											(x1, xss) = (f1 x1, xss)
						)
						
instance Monad (Sem) where
	bind (S a) f = S (\s . case a s of (as, xs) = unS (f as) xs)
	
store :: Ident Val -> Sem Val
store str v = S (\s . (v, 'Map'.put str v s))

read :: Ident -> Sem Val
read str = S (\s . (fromJust ('Map'.get str s), s))

fail :: String -> Sem String
fail str = S (\s . (str,s))

eval :: Expression -> Sem Val
eval (New 					) = pure (ValS [])
eval (Insert		e s		) = eval e >>= \(ValI i) . eval s >>= \(ValS l) . pure (ValS [i:l])
eval (Delete		e s		) = eval e >>= \(ValI i) . eval s >>= \(ValS l) . pure (ValS ('List'.difference l [i]))
eval (Variable 		str		) = read str
eval (Intersection	s1 s2	) = eval s1 >>= \(ValS l1) . eval s2 >>= \(ValS l2) . pure (ValS ('List'.intersect l1 l2))
eval (Integer		i		) = pure (ValI i)
eval (Size			s		) = eval s >>= \(ValS s). pure (ValI (length s))
eval (Oper			e1 op e2) = eval e1 >>= \v1 . eval e2 >>= \v2 . evalOp v1 v2 op
eval (=. str e				) = eval e >>= store str

evalOp :: Val Val Op -> Sem Val
evalOp (ValI i1) (ValI i2) +. = pure (ValI (i1 + i2))
evalOp (ValS l1) (ValS l2) +. = pure (ValS ('List'.union l1 l2))

evalOp (ValI i1) (ValI i2) -. = pure (ValI (i1 - i2))
evalOp (ValS l1) (ValS l2) -. = pure (ValS ('List'.difference l1 l2))

evalOp (ValI i1) (ValI i2) *. = pure (ValI (i1 * i2))
//evalOp _ _ _ 				  = 
	
print :: Expression String -> String
print (New) str = ("[]" +++ str)
print (Insert e s) str = print e (" in "+++(print s str))
print (Delete e s) str =  print e (" <- this must be removed from that ->"+++(print s str))
print (Variable st) str = st +++ str
print (Intersection s1 s2) str = print s1 (" intersected with "+++ (print s2 str))
print (Integer i) str = (toString i) +++ str
print (Size s) str = " size of(" +++ (print s (") " +++ str))
print (Oper e1 op e2) str = print e1 ((p1 op) +++ (print e2 str))
							where p1 +. = " + "
								  p1 -. = " - "
								  p1 *. = " * "
print (=. st e) str = st +++ " = " +++ (print e str)

// === simulation
(>>>=)     :== 'iTasks'.tbind
(>>>|) a b :== 'iTasks'.tbind a (\_ -> b)
treturn    :== 'iTasks'.return
ActionOk   :== 'iTasks'.ActionOk
ActionQuit :== 'iTasks'.ActionQuit
ActionNew  :== 'iTasks'.ActionNew

//sExpr :: Shared Expression
//sExpr = sharedStore "lolnoob" New

task :: State Expression -> Task Expression
task state expr = (viewInformation "lolnoob2" [] state ||- updateInformation "lolnoob" [] expr)
					>>>= \e .task (snd ((unS (eval e)) state)) e

Start :: *World -> *World
Start world = startEngine (task 'Map'.newMap ("test" =. (Oper (Integer 1) (+.) (Integer 2)))) world

//Start = viewInformation (print ("test" =. (Oper (Integer 1) (+.) (Integer 2))) "")
