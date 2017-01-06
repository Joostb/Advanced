module as11

import StdEnv

class aexpr v where
	lit :: a -> v a Expr | type a
	(+.) infixl 6 :: (v t p) (v t q) -> v t Expr
	| type, + t
	(-.) infixl 6 :: (v t p) (v t q) -> v t Expr
	| type, - t
	(*.) infixl 7 :: (v t p) (v t q) -> v t Expr
	| type, * t

//Boolean expressions
class bexpr v where
	(&.) infixr 3 :: (v Bool p) (v Bool q)->v Bool Expr
	(|.) infixr 2 :: (v Bool p) (v Bool q)->v Bool Expr
	~. :: (v Bool p) -> v Bool Expr
	(==.) infix 4 :: (v a p) (v a q) -> v Bool Expr
	| ==, type a
	(<.) infix 4 :: (v a p) (v a q) -> v Bool Expr
	| <, Ord, type a

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

rtrn :: a -> Eval a p
rtrn a = Eval \r s.(Jst a,s)

(>>-) infixl 1 :: (Eval a p) (a->Eval b q)->Eval b q
(>>-) (Eval e) f =
	Eval \r s.case e R s of
		(Jst a,t) = unEval (f a) r t
		(Err e,t) = (Err e,t)
(<*.>) infixl 4::(Eval (a->b) p) (Eval a q)->Eval b r
(<*.>) f a = f >>- \g.a >>- \b.rtrn (g b)

rwvar :: Int (RW a) State -> (MB a, State) | TC a
rwvar n R s =
	case get n s.map of
		(Just (v :: a^)) = (Jst v,s)
		(Just d) = (Err (toString n+" wrong type"),s)
		_ = (Err (toString n+" undefined"),s)
rwvar n (W a) s
	= (Jst a,{s & map = put n (dynamic a) s.map})

instance aexpr Eval where
	lit a = rtrn a
	(+.) x y = rtrn (+) <*.> x <*.> y
	(-.) x y = rtrn (-) <*.> x <*.> y
	(*.) x y = rtrn (*) <*.> x <*.> y

// BONUS
// :: Duo x y t p = Duo (x t p) (y t p)
// instance aexpr (Duo x y) | aexpr x & aexpr y
// 	where
// 	lit a = Duo (lit a) (lit a)
// 	(+.) (Duo a b) (Duo x y) = Duo (a +. x) (b +. y)
// 	(-.) (Duo a b) (Duo x y) = Duo (a -. x) (b -. y)
	// (*.) (Duo a b) (Duo x y) = Duo (a *. x) (b *. y)


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

class type a | toString a where
	type :: a -> String
instance type Int where 
	type a = "int"
instance type Bool where 
	type a = "bool"
instance type Char where 
	type a = "char"



//additional Boolean operators
// (!=.) infix 4 :: (v a p) (v a q) -> v Bool Expr | ==, type a
// (!=.) x y =  ~. (x == y)
// (>.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
// (>.) x y = y <. x
// (<=.) infix 4 :: (v a p) (v a q) -> v Bool Expr	| <, Ord, type a
// (<=.) x y = ~. (y <. x)
// (>=.) infix 4 :: (v a p) (v a q) -> v Bool Expr	| <, Ord, type a
// (>=.) x y = ~. (x <. y)

// class stmt v where
// 	If :: (v Bool p) (v a q) Else (v b r) -> v () Stmt
// 		| isExpr p
// 	While :: (v Bool p) (v a q) -> v () Stmt
// 		| isExpr p
// 	(:.) infixr 1 :: (v a p) (v b q) -> v b Stmt

// :: Else = Else

class var v where
	var2 :: ((v t Upd)->In t (v a p)) -> (v a p)
	| type t
	
:: In a b = In infix 0 a b

// fac =
// 	var2 \n. 4 In
// 	var2 \r. 1 In
// 	While (n >. lit 1) (
// 	r =. r *. n :.
// 	n =. n -. lit 1
// 	) :.
// 	r


Start = '2' + '2' 