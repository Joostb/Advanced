module assignment11

import StdEnv, StdCleanTypes, StdMisc, StdList, StdInt, Data.Tuple, StdClass, iTasks._Framework.Generic, Text.JSON, Data.Functor, Control.Applicative, Data.Void
import qualified iTasks
import qualified Text
from Text import class Text, instance Text String
from StdFunc import o
from StdTuple import fst
from Data.Map import :: Map, Bin, Tip, put, get, newMap
from Data.List import union, removeMember, instance Functor []
import qualified Data.List as List

//class lit v :: a -> v a Expr | type a

class type a | toString, TC a where
	type :: a -> String

instance type Int where type a = "int"
instance type Bool where type a = "bool"
instance type Char where type a = "char"
instance type [a] |type a where type a = "set"


class aexpr v where
	lit :: a -> v a Expr | type a
	(+.) infixl 6 :: (v t p) (v t q) -> v t Expr | type, + t
	(-.) infixl 6 :: (v t p) (v t q) -> v t Expr | type, - t
	(*.) infixl 7 :: (v t p) (v t q) -> v t Expr | type, * t
	
class bexpr v where
	(&.) infixr 3 :: (v Bool p) (v Bool q) -> v Bool Expr
	(|.) infixr 2 :: (v Bool p) (v Bool q) -> v Bool Expr
	~. :: (v Bool p) -> v Bool Expr
	(==.) infix 4 :: (v a p) (v a q) -> v Bool Expr | ==, type a
	(<.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	
	(!=.) infix 4 :: (v a p) (v a q) -> v Bool Expr | ==, type a
	(!=.) x y :==  ~. (x == y)
	(>.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	(>.) x y :== y <. x
	(<=.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	(<=.) x y :== (~. (y <. x))
	(>=.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	(>=.) x y :== ~. (x <. y)
	
class sexpr v where
	new :: v [a] Expr | type a //NEW
	(->.) infix 2 :: (v a p) (v [a] q) -> v [a] Expr | type a //INSERT
	delete :: (v a p) (v [a] q) -> v [a] Expr | type a //DELETE
	($.) infix 2 :: (v [a] p) (v [a] q) -> v [a] Expr | type a //UNION
	(--.) infix 2 :: (v [a] p) (v [a] q) -> v [a] Expr | type a //DIFFERENCE
	(\.) infix 2 :: (v [a] p) (v [a] q) -> v [a] Expr | type a //INTERSECT

:: Upd = Upd
:: Expr = Expr
:: Stmt = Stmt

class isStmt a :: a -> a
instance isStmt Stmt where isStmt a = a
instance isStmt Expr where isStmt a = a
instance isStmt Upd where isStmt a = a

class isExpr a :: a -> a
instance isExpr Expr where isExpr a = a
instance isExpr Upd where isExpr a = a

class stmt v where
	If :: (v Bool p) (v a q) Else (v b r) -> v () Stmt | isExpr p
	While :: (v Bool p) (v a q) -> v () Stmt | isExpr p
	(:.) infixr 1 :: (v a p) (v b q) -> v b Stmt
:: Else = Else

class var v where
	(=.) infixr 2 :: (v t Upd) (v t p) -> v t Expr | type t & isExpr p
	var :: t ((v t Upd) -> (v a p)) -> (v a p) | type t
	var2 :: ((v t Upd) -> In t (v a p)) -> (v a p) | type t

:: In a b = In infix 0 a b

fac =
	var2 \n. [4] In
	var2 \n. 4 In
	var2 \r. 1 In
	While (n >. lit 1) (
		r =. r *. n :.
		n =. n -. lit 1
	) :.
	r
	
// -------------------------
// SHOOOOOOOOW TIME!!!~!111!ONE1ELEVEN!!
// -------------------------

:: Show a p = Show (SHOW -> SHOW)
:: SHOW =
	{ fresh :: Int
	, indent :: Int
	, print :: [String]
	}
	
s0 :: SHOW
s0 =
	{ fresh = 0
	, indent = 0
	, print = ["\n"]
	}
	
unShow :: (Show a p) -> (SHOW -> SHOW)
unShow (Show f) = f 
	
c :: a -> Show b c | toString a
c a = Show \c.{c & print = [toString a:c.print]}

(+.+) infixl 5 :: (Show a p) (Show b q) -> Show c r
(+.+) (Show f) (Show g) = Show (g o f) 

fresh :: (Int -> (Show a p)) -> (Show a p)
fresh f = Show \c.unShow (f c.fresh) {c & fresh = inc c.fresh}

freshVar :: ((Show b q) -> (Show a p)) -> (Show a p)
freshVar f = fresh (f o \n.c ("v" +++ (toString n) ))

indent :: Show a b
indent = Show \c.{c & indent = inc c.indent}

unindent :: Show a b
unindent = Show \c.{c & indent = c.indent - one}

nl :: Show a b
nl = Show \c.{c & print = [toString ['\n':repeatn (2 * c.indent) ' ']:c.print]}

brac :: (Show a p) -> Show b q
brac e = c "(" +.+ e +.+ c ")"

instance aexpr Show where
	lit a = c a
	(+.) x y = brac (x +.+ c "+" +.+ y)
	(-.) x y = brac (x +.+ c "-" +.+ y)
	(*.) x y = brac (x +.+ c "*" +.+ y)
	
instance bexpr Show where
	(&.) x y = brac ( x +.+ c " AND " +.+ y)
	(|.) x y = brac ( x +.+ c " OR " +.+ y)
	~. x = brac ( c "NOT " +.+ x)
	(==.) x y = brac ( x +.+ c " EQUALS " +.+ y)
	(<.) x y = brac ( x +.+ c " LESSER THAN " +.+ y)
	
instance sexpr Show where
	new = c "[]"
	(->.) x y = x +.+ c " U+2208 " +.+ y
	delete x y = x +.+ c " DELETE FROM " +.+ y
	($.) x y = x +.+ c " MASH TOGETHER WITH " +.+ y
	(--.) x y = x +.+ c " DIFFERENCE " +.+ y
	(\.) x y = x +.+ c " INTERSECT " +.+ y 
	
instance stmt Show where
	(:.) s t = s +.+ c ";" +.+ nl +.+ t +.+ nl
	While b s = c "while " +.+ b +.+ c " {" +.+ indent +.+ nl +.+ s 
				+.+ unindent +.+ nl +.+ c "}" +.+ nl
	If b t else e = c "if " +.+ b +.+ c " {" +.+ indent +.+ nl +.+ t 
					+.+ unindent +.+ nl +.+ c "} else {" +.+ indent 
					+.+ nl +.+ e +.+ unindent +.+ nl +.+ c "}" +.+ nl
					
instance var Show where
	(=.) v e = v +.+ c " = " +.+ e
	var x f = c (type x) +.+ c " " +.+ freshVar \v. v +.+
				c " = " +.+ c x +.+ c ";" +.+ nl +.+ f v
	var2 f = freshVar \v. 
				let (x In rest) = f v in c (type x) +.+ c " " +.+ v 
									+.+ c " = " +.+ c x +.+ c ";" +.+ nl +.+ rest
									
show :: (Show a p) -> [String] | type a
show (Show f) = reverse (f s0).print

//
// EVAL
//

:: Eval a p = Eval ((RW a) State -> (MB a,State))
:: RW a = R | W a
:: MB a = Jst a | Err String
:: State =
	{ map :: Map Int Dynamic
	, vars :: Int
	}
e0 :: State
e0 =
	{ map = newMap
	, vars = 0
	}
	
unEval :: (Eval a p) -> (RW a) State -> (MB a,State)
unEval (Eval f) = f
	
rtrn :: a -> Eval a p
rtrn a = Eval \r s.(Jst a,s)

(>>-) infixl 1 :: (Eval a p) (a -> Eval b q) -> Eval b q
(>>-) (Eval e) f = Eval \r s.case e R s of
							(Jst a,t) = unEval (f a) r t
							(Err e,t) = (Err e,t)
							
(<*.>) infixl 4::(Eval (a -> b) p) (Eval a q) -> Eval b r
(<*.>) f a = f >>- \g.a >>- \b.rtrn (g b)

rwvar :: Int (RW a) State -> (MB a, State) | TC a
rwvar n R s = case get n s.map of
				(Just (v :: a^)) = (Jst v,s)
				(Just d) = (Err (toString n+++" wrong type"),s)
				_ = (Err (toString n+++" undefined"),s)
rwvar n (W a) s
= (Jst a,{s & map = put n (dynamic a) s.map})

instance aexpr Eval where
	lit a = rtrn a
	(+.) x y = rtrn (+) <*.> x <*.> y
	(-.) x y = rtrn (-) <*.> x <*.> y
	(*.) x y = rtrn (*) <*.> x <*.> y
	
	/*
		(&.) infixr 3 :: (v Bool p) (v Bool q) -> v Bool Expr
	(|.) infixr 2 :: (v Bool p) (v Bool q) -> v Bool Expr
	~. :: (v Bool p) -> v Bool Expr
	(==.) infix 4 :: (v a p) (v a q) -> v Bool Expr | ==, type a
	(<.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	*/
	
instance bexpr Eval where
	(==.) x y = rtrn (==) <*.> x <*.> y
	(&.) x y = rtrn (&&) <*.> x <*.> y
	(|.) x y = rtrn (||) <*.> x <*.> y
	~. x = rtrn (not) <*.> x
	(<.) x y = rtrn (<) <*.> x <*.> y
	
instance stmt Eval where
	(:.) s t = s >>- \_.toStmt t
	If b t else e = b >>- \c.if c (t >>- \_.rtrn ()) (e >>- \_.rtrn ())
	While b s = b >>- \c.if c (s :. While b s) (rtrn ())
	
toStmt :: (Eval t p) -> Eval t Stmt
toStmt (Eval f) = Eval f

instance var Eval where
	(=.) v e = e >>- \a.Eval \r s.unEval v (W a) s
	var x f = Eval \r s.unEval (f (Eval (rwvar s.vars)))
				R 	{s & vars = inc s.vars
					, map = put s.vars (dynamic x) s.map
					}
		
	var2 f = Eval \r s. let (x In (Eval rest))=f (Eval (rwvar s.vars))
			in rest R 	{s & vars = inc s.vars
						, map = put s.vars (dynamic x) s.map
						} 
						
eval :: (Eval a p) -> [String] | type a
eval (Eval f) = case fst (f R e0) of
				Jst a = [toString a,"\n"]
				Err e = ["Error: ",e,"\n"]

//START
Start = eval fac
