module assignment9

import StdCleanTypes, StdMisc, StdList, StdInt, Data.Tuple, StdClass, iTasks._Framework.Generic, Text.JSON, Data.Functor, Control.Applicative, Data.Void
from iTasks import always, hasValue, :: TaskValue(..), :: Task, :: Stability, :: TaskCont(..), :: Action, updateInformation, 
		viewInformation, class descr, instance descr String, :: UpdateOption, :: ViewOption(..), -||-, -||, ||-, startEngine, 
		class Publishable, >>*, class TFunctor, instance TFunctor Task, class TApplicative, instance TApplicative Task, 
		instance Publishable Task, Void, sharedStore, ::Shared, ::ReadWriteShared, Action, ::ActionOption, ::ActionName, upd,
		updateSharedInformation, viewSharedInformation
from StdEnv import &&, ||
import qualified iTasks
import iTasks.API.Core.Types
import qualified Text
from Text import class Text, instance Text String
from StdFunc import o
from StdTuple import snd, fst
from Data.Map import :: Map, Bin, Tip, put, get, newMap
from Data.List import union, removeMember, instance Functor []
import qualified Data.List as List

//class lit v :: a -> v a Expr | type a

class type a | toString, TC a where
	type :: a -> String

:: Button = Select | Up | Left | Down | Right
// :: Display a = D a 

instance type Int where type a = "int"
instance type Bool where type a = "bool"
instance type String where type a = "string"
instance type () where type a = "()"

instance toString () where toString _ = "()"

instance toString Button where
	toString Select = "keyVals[0]"
	toString Up = "keyVals[1]"
	toString Down = "keyVals[2]"
	toString Right = "keyVals[3]"
	toString Left = "keyVals[4]"



class aexpr v where
	lit :: a -> v a Expr | type a
	(+.) infixl 6 :: (v t p) (v t q) -> v t Expr | type, + t
	(-.) infixl 6 :: (v t p) (v t q) -> v t Expr | type, - t
	(*.) infixl 7 :: (v t p) (v t q) -> v t Expr | type, * t
	
class bexpr v where
	button :: Button -> v Bool Expr
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
	SetUp :: (v a q) -> v () Stmt
	Loop :: (v a q) -> v () Stmt
	Print :: (v String q) -> v () Stmt
:: Else = Else

/*class lcd v where
	PrintUp :: (v String p) -> v () Stmt | isExpr p
	PrintDown :: (v String p) -> v () Stmt | isExpr p
*/
class var v where
	(=.) infixr 2 :: (v t Upd) (v t p) -> v t Expr | type t & isExpr p
	var :: t ((v t Upd) -> (v a p)) -> (v a p) | type t
	var2 :: ((v t Upd) -> In t (v a p)) -> (v a p) | type t

:: In a b = In infix 0 a b


	
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

setUp = c "#define KEY_COUNT 5" +.+ nl +.+ 
		c "int keyLimits [KEY_COUNT+1] = {50, 190, 380, 555, 790, 1024};" +.+ nl +.+
		c "bool keyVals [KEY_COUNT + 1] = {false, false, false, false, false};" +.+ nl +.+
		c "LiquidCrystal lcd = LiquidCrystal(8, 9, 4, 5, 6, 7);" 

setFalse = c "for (int i=0; i<KEY_COUNT; i+=1){  keyVals[i] = false; }"
assignButtons = c "int val = analogRead(A0); for (int i = 0; i < KEY_COUNT; i += 1) { if (val < keyLimits[i]) { keyVals[i] = true; break; }}"

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
	button b = c (toString b)
	(&.) x y = brac ( x +.+ c " && " +.+ y)
	(|.) x y = brac ( x +.+ c " || " +.+ y)
	~. x = brac ( c " ! " +.+ x)
	(==.) x y = brac ( x +.+ c " == " +.+ y)
	(<.) x y = brac ( x +.+ c " < " +.+ y)
	

instance stmt Show where
	(:.) s t = s +.+ c ";" +.+ nl +.+ t +.+ c ";" +.+ nl
	While b s = c "while " +.+ b +.+ c " {" +.+ indent +.+ nl +.+ s 
				+.+ unindent +.+ nl +.+ c "}" +.+ nl
	If b t else e = c "if " +.+ b +.+ c " {" +.+ indent +.+ nl +.+ t 
					+.+ unindent +.+ nl +.+ c "} else {" +.+ indent 
					+.+ nl +.+ e +.+ unindent +.+ nl +.+ c "}" +.+ nl
	SetUp stmt = setUp +.+ c "void setUp() {" +.+ nl +.+ setFalse +.+ nl +.+ assignButtons +.+ nl
						 +.+ nl +.+ indent +.+ stmt  +.+ unindent +.+ nl 
						 +.+ c "}" +.+ nl
	Loop stmt = c "void Loop() {"
					 +.+ nl +.+ indent +.+ stmt  +.+ unindent +.+ nl 
					 +.+ c "}" +.+ nl
	Print str = c "Print(" +.+ str +.+ c ")" +.+ nl

/*instance lcd Show where
	PrintUp expr = c "lcd(" +.+ expr +.+ c ")" +.+ nl
	PrintDown expr = c "lcd(" +.+ expr +.+ c ")" +.+ nl
*/
					
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
	, tstate :: TState
	}

state0 :: TState -> State
state0 ts =
		{map = newMap
		, vars = 0
		, tstate = ts
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



printlcd :: [Char] -> Eval () p
printlcd str 
	| length str > 32 	= let (str1, str2) = splitAt 16 str in Eval \r s.(Jst (), {s & tstate.lcd1 = toString str1, tstate.lcd2 = toString (take 16 str2)})
	| length str >= 16	= let (str1, str2) = splitAt 16 str in Eval \r s.(Jst (), {s & tstate.lcd1 = toString str1, tstate.lcd2 = toString (str2 ++ repeatn (16 - length str2) '-')})
	| otherwise			= Eval \r s.(Jst (), {s & tstate.lcd1 = toString (str ++ repeatn (16 - length str) '-')})


instance aexpr Eval where
	lit a = rtrn a
	(+.) x y = rtrn (+) <*.> x <*.> y
	(-.) x y = rtrn (-) <*.> x <*.> y
	(*.) x y = rtrn (*) <*.> x <*.> y
	
instance bexpr Eval where
	button b = rtrn True //TODO, check if the button is pressed
	(==.) x y = rtrn (==) <*.> x <*.> y
	(&.) x y = rtrn (&&) <*.> x <*.> y
	(|.) x y = rtrn (||) <*.> x <*.> y
	~. x = rtrn (not) <*.> x
	(<.) x y = rtrn (<) <*.> x <*.> y
	
instance stmt Eval where
	(:.) s t = s >>- \_.toStmt t
	If b t else e = b >>- \c.if c (t >>- \_.rtrn ()) (e >>- \_.rtrn ())
	While b s = b >>- \c.if c (s :. While b s) (rtrn ())
	SetUp stmt = stmt >>- \_. (rtrn ()) // een keer uitvoeren
	Loop stmt = stmt >>- \_. (rtrn ()) //100000000000000 keer uitvoeren
	Print stre = stre >>- \str. printlcd (fromString str)
	
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
						
eval :: (Eval a p) TState -> [String] | type a
eval (Eval f) s = case fst (f R (state0 s)) of
				Jst a = [toString a,"\n"]
				Err e = ["Error: ",e,"\n"]
				
eval2 :: (Eval a p) TState -> ([String], State) | type a
eval2 (Eval f) s = case f R (state0 s) of
				(Jst a, s) = ([toString a,"\n"], s)
				(Err e, s) = (["Error: ",e,"\n"], s)
				
evalState :: (Eval a p) TState -> State
evalState (Eval f) s = snd (f R (state0 s))


fac = Loop (Print (lit "csdnfjsdf"))
	
  	 

//ITASK DINGES

// task :: 

:: TState = {up::Bool, down::Bool, right::Bool, left::Bool, select ::Bool, lcd1 :: String, lcd2 :: String}

derive class iTask TState 

state = sharedStore "sharedState" {left = False, right = False, up = False, down = False, select = False, lcd1 = "1234567890abcdef", lcd2 = "1234567890abcdef"}


buttonTask ::  Task TState
buttonTask  = viewSharedInformation "BUTTONS" [] state
							>>* [OnAction (Action "left" []) (always (upd (\s . {s & left = not s.left}) state ||- buttonTask )),OnAction (Action "right" []) (always (upd (\s . {s & right = not s.right}) state ||- buttonTask )),OnAction (Action "up" []) (always (upd (\s . {s & up = not s.up}) state ||- buttonTask )),OnAction (Action "down" []) (always (upd (\s . {s & down = not s.down}) state ||- buttonTask )),OnAction (Action "select" []) (always (upd (\s . {s & select = not s.select}) state ||- buttonTask ))]

executeTask f = ('iTasks'.get state >>= \s . upd 
											(let s2 = (evalState f s) in \ts . {ts & lcd1 = s2.tstate.lcd1, lcd2 = s2.tstate.lcd2}) 
											state)

testprog = Print (lit "hello world, i like money, lol xD i am so random")

dinges :: *World -> *World
dinges world = startEngine (executeTask testprog ||- buttonTask) world

//START
Start :: *World -> * World
Start world = dinges world

// Start = show fac

//Start = foldl (\a b. a +++ b) " " (show fac)



// MEUK

// imageTask = updateSharedInformation 
// 				(Title "LEFT")
// 				[ImageUpdate
// 					id // server state (share) and view are identical
// 					(\s v tags . myImage s) // generate image
// 					(\s v . v) //update view when state changes
// 					(\s v . s) // update state when view changes
// 					(\_ s . Nothing) //no conflict handling
// 					(\o n . n) //always select new state
// 					] 
// 					state

// myImage :: TState -> Image TState
// myImage s = 
// 		overlay [(AtMiddleX,AtMiddleY)] [] [text font "left"]
// 		(Just (circle (px 100.0)
// 			<@< {onclick = \i s . {s & left = True}, local = False}))
