module assignment9

import StdCleanTypes, StdMisc, StdList, StdInt, Data.Tuple, StdClass, iTasks._Framework.Generic, Text.JSON, Data.Functor, Control.Applicative, Data.Void
from iTasks import always, ifValue, hasValue, :: TaskValue(..), :: Task, :: Stability, :: TaskCont(..), :: Action, updateInformation, 
		viewInformation, class descr, instance descr String, :: UpdateOption, :: ViewOption(..), -||-, -||, ||-, startEngine, 
		class Publishable, >>*, class TFunctor, instance TFunctor Task, class TApplicative, instance TApplicative Task, 
		instance Publishable Task, Void, sharedStore, ::Shared, ::ReadWriteShared, Action, ::ActionOption, ::ActionName, upd,
		updateSharedInformation, viewSharedInformation, class tune, <<@, instance tune ArrangeHorizontal, ArrangeHorizontal, 
		:: ArrangeHorizontal, enterChoice, :: ChoiceOption, waitForTime, waitForTimer, repeatTask
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


class type a | toString, TC a where
	type :: a -> String

:: Button = Select | Up | Left | Down | Right

instance type Int where type a = "int"
instance type Bool where type a = "bool"
instance type String where type a = "string"
instance type () where type a = "()"

//Tostring2 print in arduinoC ready style:
// True -> true, a string "sss" is printed with quotes.
class toString2 a where
	toString2 :: a -> String

instance toString2 Int where
	toString2 a = toString a

instance toString2 Bool where
	toString2 True = "true"
	toString2 False = "false"

instance toString2 String where
	toString2 a = "\"" +++ a +++ "\""

instance toString2 () where
	toString2 a = toString a

instance toString () where toString _ = "()"

//Provides access to buttons in arduinoC
instance toString Button where
	toString Select = "keyVals[0]"
	toString Up = "keyVals[1]"
	toString Down = "keyVals[2]"
	toString Right = "keyVals[3]"
	toString Left = "keyVals[4]"

class aexpr v where
	lit :: a -> v a Expr | type, toString2 a
	millis :: v Int Expr
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
	Print :: (v a q) -> v () Stmt | type a
	PrintReset :: v () Stmt
	Cmt :: (v String p) -> v () Stmt
	Empty :: v () Stmt

:: Else = Else

class var v where
	(=.) infixr 2 :: (v t Upd) (v t p) -> v t Expr | type t & isExpr p
	var :: t ((v t Upd) -> (v a p)) -> (v a p) | type, toString2 t
	var2 :: ((v t Upd) -> In t (v a p)) -> (v a p) | type, toString2 t

:: In a b = In infix 0 a b


	
// -------------------------
// Show view
// Generates C that should run on the arduino
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

// Setup to access the buttons
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
	lit a = Show \c.{c & print = [toString2 a:c.print]}
	millis = c "millis()" 
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
				+.+ unindent +.+ nl +.+ c ";}" +.+ nl
	If b t else e = c "if " +.+ b +.+ c " {" +.+ indent +.+ nl +.+ t 
					+.+ unindent +.+ nl +.+ c ";} else {" +.+ indent 
					+.+ nl +.+ e +.+ unindent +.+ nl +.+ c ";}" +.+ nl
	SetUp stmt = setUp +.+ nl +.+ nl +.+ c "void setup() {" +.+ indent +.+ nl 
						 +.+ setFalse +.+ nl +.+ assignButtons +.+ nl +.+ nl
						 +.+ stmt  +.+ unindent +.+ nl 
						 +.+ c ";}" +.+ nl
	Loop stmt = c "void loop() {"
					 +.+ nl +.+ indent +.+ stmt  +.+ unindent +.+ nl 
					 +.+ c ";}" +.+ nl
	Print str = c "lcd.print( " +.+ str +.+ c ");" +.+ nl
	PrintReset = c "lcd.setCursor(0, 0);" +.+ nl
	Cmt str = c "////" +.+ str +.+ nl
	Empty = Show \c . c
					
instance var Show where
	(=.) v e = v +.+ c " = " +.+ e
	var x f = c (type x) +.+ c " " +.+ freshVar \v. v +.+
				c " = " +.+ (Show \c.{c & print = [toString2 x:c.print]}) +.+ c ";" +.+ nl +.+ f v
	var2 f = freshVar \v. 
				let (x In rest) = f v in c (type x) +.+ c " " +.+ v 
									+.+ c " = " +.+ (Show \c.{c & print = [toString2 x:c.print]}) 
									+.+ c ";" +.+ nl +.+ rest
									
show :: (Show a p) -> [String] | type a
show (Show f) = reverse (f s0).print

//
// The view that generates the iTask thing
//

:: Eval a p = Eval ((RW a) State -> (MB a,State))
:: RW a = R | W a
:: MB a = Jst a | Err String
:: State =
	{ stime :: Time
	, ctime :: Time
	, loop :: Eval () Stmt
	, map :: Map Int Dynamic
	, vars :: Int
	, cursor :: Int
	, tstate :: TState
	}
	
derive class iTask State
derive class iTask Stmt
derive class iTask Eval
derive class iTask RW
derive class iTask MB

state0 :: Time TState -> State
state0 t ts =
		{ stime = t
		, ctime = t
		, loop = Loop(lit 0)
		, map = newMap
		, vars = 0
		, cursor = 0
		, tstate = ts
		}
	
unEval :: (Eval a p) -> (RW a) State -> (MB a,State)
unEval (Eval f) = f
	
rtrn :: a -> Eval a p
rtrn a = Eval \r s.(Jst a,s)

rtrnbttn :: Button -> Eval Bool p
rtrnbttn b = Eval \r s.(Jst (case b of
								Left = s.tstate.left
								Right = s.tstate.right
								Up = s.tstate.up
								Down = s.tstate.down
								Select = s.tstate.select),s)

rtrnloop :: (Eval a p) -> Eval () Stmt
rtrnloop stmt = Eval \r s.(Jst (),{s & loop = Loop(stmt)})

rtrnmillis :: Eval Int p
rtrnmillis = Eval \r s.(Jst (let {sec=sec, min=m, hour=h} = (s.ctime - s.stime) in (sec + m*60 + h*60*60)*1000),s)

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


print :: a -> Eval () p | type a
print a = Eval \r s.(Jst (), (printchars (fromString (toString a)) s))

printchars :: [Char] State -> State
printchars [] s = s
printchars [c:str] s 
		| s.cursor < 16 = printchars str {s & tstate.lcd1 = s.tstate.lcd1 +++ (toString c), cursor = s.cursor + 1}
		| s.cursor < 32 = printchars str {s & tstate.lcd2 = s.tstate.lcd2 +++ (toString c), cursor = s.cursor + 1}
		| otherwise		= s

printreset :: Eval () p
printreset = Eval \r s . (Jst (), {s & tstate.lcd1 = "", tstate.lcd2 = "", cursor=0})

instance aexpr Eval where
	lit a = rtrn a
	millis = rtrnmillis
	(+.) x y = rtrn (+) <*.> x <*.> y
	(-.) x y = rtrn (-) <*.> x <*.> y
	(*.) x y = rtrn (*) <*.> x <*.> y
	
instance bexpr Eval where
	button b = rtrnbttn b
	(==.) x y = rtrn (==) <*.> x <*.> y
	(&.) x y = rtrn (&&) <*.> x <*.> y
	(|.) x y = rtrn (||) <*.> x <*.> y
	~. x = rtrn (not) <*.> x
	(<.) x y = rtrn (<) <*.> x <*.> y
	
instance stmt Eval where
	(:.) s t = s >>- \_.toStmt t
	If b t else e = b >>- \c.if c (t >>- \_.rtrn ()) (e >>- \_.rtrn ())
	While b s = b >>- \c.if c (s :. While b s) (rtrn ())
	SetUp stmt = stmt >>- \_. (rtrn ()) // is ran once
	Loop stmt = stmt >>- \_. (rtrnloop stmt) // is ran every step
	Print stre = stre >>- \str. print str
	PrintReset = printreset
	Cmt str = rtrn ()
	Empty = rtrn ()
	
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
						
eval :: (Eval a p) Time TState -> [String] | type a
eval (Eval f) t s = case fst (f R (state0 t s)) of
				Jst a = [toString a,"\n"]
				Err e = ["Error: ",e,"\n"]
				
eval2 :: (Eval a p) Time TState -> ([String], State) | type a
eval2 (Eval f) t s = case f R (state0 t s) of
				(Jst a, s) = ([toString a,"\n"], s)
				(Err e, s) = (["Error: ",e,"\n"], s)
				
evalState :: (Eval a p) Time TState -> State
evalState (Eval f) t s = snd (f R (state0 t s))

evalLoop :: State -> State
evalLoop (s=:{loop=(Eval f)}) = snd (f R s)


//
// A view that checks if the language written is correct.
// Checks if their is exactly one setup and on loop statement
//
:: Check a p = Check (CHECK -> CHECK)
:: CHECK =
	{ loops :: Int
	, setups :: Int
	}
	
sCheck :: CHECK
sCheck =
	{ loops = 0
	, setups = 0
	}

unCheck :: (Check a p) -> (CHECK -> CHECK)
unCheck (Check f) = f 
	
// throws away the a, and does nothing
c1 :: a -> Check b c1 | toString a
c1 a = Check \c.c

// Combines two statements
(+.=.+) infixl 5 :: (Check a p) (Check b q) -> Check c1 r
(+.=.+) (Check f) (Check g) = Check (g o f) 

instance aexpr Check where
	lit a = c1 a
	millis = c1 "no setup or loop here"
	(+.) x y = c1 "No setup or loop here"
	(-.) x y = c1 "No setup or loop here"
	(*.) x y = c1 "No setup or loop here"
	
instance bexpr Check where
	button b = c1 "No setup or loop here"
	(&.) x y = c1 "No setup or loop here"
	(|.) x y = c1 "No setup or loop here"
	~. x = c1 "No setup or loop here"
	(==.) x y = c1 "No setup or loop here"
	(<.) x y = c1 "No setup or loop here"

instance stmt Check where
	(:.) s t =  s +.=.+ t
	While b s =  c1 "No setup or loop here"
	If b t else e =  c1 "No setup or loop here"
	SetUp stmt = Check \c.{c & setups = c.setups + 1}
	Loop stmt = Check \c.{c & loops = c.loops + 1}
	Print str = c1 "No setup or loop here"
	PrintReset = c1 "No setup or loop here"
	Cmt str	= Check \c . c 
	Empty 	= Check \c . c

checkVar :: ((Check b q) -> (Check a p)) -> (Check a p)
checkVar f =  Check \state . unCheck (f (c1 "bloeb")) state

instance var Check where
	(=.) v e =  c1 "No setup or loop here"
	var x f =  checkVar \v. v +.=.+ f v
	var2 f = checkVar \v. 
			let (x In rest) = f v in  v +.=.+ rest
									
check :: (Check a p) -> [String] | type a
check (Check f) = if ((checked.loops == 1) && (checked.setups == 1)) ["OKE"] ["NOT OK", " Number of loops ", toString checked.loops]
					where checked = f sCheck

	 
//
// ITASK Setup and construction
//

:: TState = {up::Bool, down::Bool, right::Bool, left::Bool, select ::Bool, lcd1 :: String, lcd2 :: String, stop :: Bool, step :: Int}

derive class iTask TState 

state = sharedStore "sharedState" {left = False, right = False, up = False, down = False, select = False, lcd1 = "1234567890abcdef", lcd2 = "1234567890abcdef", stop=False, step=0}


buttonTask ::  Task TState
buttonTask  = viewSharedInformation "BUTTONS" [] state
							>>* [OnAction (Action "left" []) (always (upd (\s . {s & left = not s.left}) state ||- buttonTask )),OnAction (Action "right" []) (always (upd (\s . {s & right = not s.right}) state ||- buttonTask )),OnAction (Action "up" []) (always (upd (\s . {s & up = not s.up}) state ||- buttonTask )),OnAction (Action "down" []) (always (upd (\s . {s & down = not s.down}) state ||- buttonTask )),OnAction (Action "select" []) (always (upd (\s . {s & select = not s.select}) state ||- buttonTask ))]

ChooseView = enterChoice "Choose a program" [] ["Score Counter", "Countdown", "TimeTest"]
				>>* [OnAction ActionOk (hasValue (\s . case s of 
														"Score Counter" = return scoreCounter
														"Countdown"		= return countDown
														"TimeTest"		= return (Loop (PrintReset :. Print(millis)))
														))]
				>>= \f. StartTask f >>= \s . StartView s
				
StartView :: State -> Task State
StartView s = 	viewInformation "Options" [] "Select your option"
				>>* [OnAction (Action "Back" []) (always (ChooseView))
					,OnAction (Action "Step" []) (always (StepTask s >>= \s2 . StartView s2))
					,OnAction (Action "Run" []) (always (RunTask s >>= \s2 . StartView s2))]


RunTask (s1=:{tstate})
		| not tstate.stop = (((viewInformation "wub" [] "wub" >>* [OnAction (Action "Back" []) (always (upd (\s . {s & stop = True}) state))]) ||- waitForTimer {hour=0, min=0, sec=0}) >>| StepTask s1) >>= \s2 . RunTask s2
		| otherwise = upd (\s1 . {s1 & stop = False}) state >>= \ts . return {s1 & tstate=ts}

				
StartTask f = 'iTasks'.get 'iTasks'.currentTime >>= \ct . 'iTasks'.get state >>= \ts . (let s2 = (evalState f ct ts) in 
											upd 
											(\ts2 . {ts2 & lcd1 = s2.tstate.lcd1, lcd2 = s2.tstate.lcd2}) 
											state >>| return s2)
			
		
StepTask :: State -> Task State					
StepTask s1 = 'iTasks'.get 'iTasks'.currentTime >>= \ct . 'iTasks'.get state >>= \ts . (let s2 = (evalLoop {s1 & ctime = ct, tstate = ts}) in 
											upd 
											(\ts . {ts & lcd1 = s2.tstate.lcd1, lcd2 = s2.tstate.lcd2}) 
											state >>| return s2
											)

dinges :: *World -> *World
dinges world = startEngine ((ChooseView ||- buttonTask <<@ ArrangeHorizontal) <<@ ArrangeHorizontal) world

//START
// Multiple starts for the different views 
Start :: *World -> * World
Start world = dinges world

//Start = foldl (\a b. a +++ b) " " (show countDown)
//Start = foldl (\a b. a +++ b) " " (check test)

//////////// The desired programs ///////////////

fac = Loop (Print (lit "csdnfjsdf")) :. SetUp (lit "r")


scoreCounter = 
			var2 \teamOne . 0 In
			var2 \teamTwo . 0 In
			var2 \test . toString2 "test" In
			SetUp (Empty) :.
			Loop (
				PrintReset :.
				If (button Left) (
					teamOne =. teamOne +. lit 1
				)
				Else (
					If (button Right) (
						teamTwo =. teamTwo +. lit 1
					)
					Else (
						lit True
					)
				) :.

				If (button Down) (
						teamTwo =. lit 0 :.
						teamOne =. lit 0
					) Else (lit 0) :.
				Print(teamOne) :.
				Print(lit "-") :.
				Print(teamTwo)
			)

countDown = 
 var2 \ minutes. 0 In
 var2 \ seconds. 0 In
 var2 \ lastTime . 0 In
 var2 \ start. False In
 SetUp ( 
 	lastTime =. millis :.
 	PrintReset
 ) :.

 Loop (
 	If (~. start) (
 		// Set the time.
 		If (button Left) (
 			minutes =. minutes +. lit 1
 		) Else (lit "Leeg") :.
 		If (button Right) (
 			seconds =. seconds +. lit 5
 		) Else (lit "Leeg") :.
 		If (button Select) (
 			start =. lit True
 		) Else (lit "leeg") :.
 		PrintReset :.

 		Print (lit "m:s") :.
 		Print (minutes) :.
 		Print (lit "-") :.
 		Print (seconds)
 	) Else (

	 	If ((lastTime -. millis) >=. lit 1000 ) ( // A second passed
	 		lastTime =. millis :.
	 		If (seconds ==. lit 0) (
	 			If (~. (minutes ==. lit 0)) (
	 				seconds =. lit 59 :.
	 				minutes =. minutes -. lit 1
	 			) Else (
	 				PrintReset :.
	 				Print(lit "Time's up")
	 			)
	 		) Else (
	 			seconds =. seconds -. lit 1 :.
	 			Print(lit "m:s") :.
	 			Print(minutes) :.
	 			Print(lit ":") :.
	 			Print(seconds)
	 		)
	 	) Else ( lit "No second passed")
 	)
 )
