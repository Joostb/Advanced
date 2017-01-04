implementation module types

/*
	Pieter Koopman pieter@cs.ru.nl
	Advanced programming, advanced iTask, 2016
	Demo program for SVG and a share
	
	This is an iTask program:
	- use environment iTasks
	- place the executable in the iTasks-SDK folder or a subfolder of it
	- the SVG system can use much memory, for instance 50M (in project options)
*/

import iTasks
import iTasks.API.Extensions.SVG.SVGlet

instance == UserRole where
	(==) Master Master = True
	(==) Designer Designer = True
	(==) Machinist Machinist = True
	(==) Controller Controller = True
	(==) _ _ = False
	
instance == Orientation where
	(==) NE NE = True
	(==) NW NW = True
	(==) SE SE = True
	(==) SW SW = True
	(==) _ _ = False
	
instance == Position where
	(==) {xPos=x, yPos=y} {xPos, yPos} = x==xPos && y==yPos


derive class iTask Position
derive class iTask Orientation

derive class iTask Switch
derive class iTask Section
derive class iTask Type
derive class iTask Track

derive class iTask Train

derive class iTask UserRole
derive class iTask UserSettings

derive class iTask State

GetRoleSettings :: UserRole -> UserSettings
GetRoleSettings Master = {uShowGrid = True, uShowTrain = True, uTrainDriver = Nothing, uMoveTrain = True,	uToggleLights = True, uToggleSwitches = True}
GetRoleSettings Designer = {uShowGrid = True, uShowTrain = True, uTrainDriver = Nothing, uMoveTrain = False,	uToggleLights = False, uToggleSwitches = False}
GetRoleSettings Machinist = {uShowGrid = False, uShowTrain = True, uTrainDriver = Nothing, uMoveTrain = False, uToggleLights = False, uToggleSwitches = False}
GetRoleSettings Controller = {uShowGrid = False, uShowTrain = True, uTrainDriver = Nothing, uMoveTrain = False, uToggleLights = True, uToggleSwitches = True}


isNorth :: Orientation -> Bool
isNorth orin = isMember orin [NE, NW]

isEast :: Orientation -> Bool
isEast orin = isMember orin [NE, SE]

isSouth :: Orientation -> Bool
isSouth orin = isMember orin [SE, SW]

isWest :: Orientation -> Bool
isWest orin = isMember orin [SW, NW]

isSection :: Track -> Bool
isSection {tType=(SEC _ )} = True
isSection _ = False

isSwitch :: Track -> Bool
isSwitch t = not (isSection t)

TrainEq :: Train Train -> Bool
TrainEq {inipos, despos} {inipos=inipos2, despos=despos2} = inipos == inipos2 && despos==despos2

ExistsTrackByPosition :: [Track] Position -> Bool
ExistsTrackByPosition ts {xPos=x, yPos=y} = length (valid) > 0
		where 	valid = filter (\{tPosition={xPos, yPos}, tType} . case tType of
										(SEC section) = x == xPos && y == yPos 
										(SWT {sOrientation}) = (x == xPos && y == yPos) ||
																(x == xPos && (y+1) == yPos && isSouth sOrientation) ||
																(x == xPos && (y-1) == yPos && isNorth sOrientation))
								ts

GetTrackByPosition :: [Track] Position -> Track
GetTrackByPosition ts {xPos=x, yPos=y} = (valid)!!0
		where	valid = filter (\{tPosition={xPos, yPos}, tType} . case tType of
										(SEC section) = x == xPos && y == yPos 
										(SWT {sOrientation}) = (x == xPos && y == yPos) ||
																(x == xPos && (y+1) == yPos && isSouth sOrientation) ||
																(x == xPos && (y-1) == yPos && isNorth sOrientation))
								ts


ToggleSwitch :: [Track] Int -> [Track]
ToggleSwitch ts i = case (ts!!i).tType of
					(SWT swt) = updateAt i {ts!!i & tType = (SWT {swt & sOn=(not swt.sOn)})} ts
					
ToggleLight :: [Track] Bool Int -> [Track]
ToggleLight ts left i = case (ts!!i).tType of
							(SEC sec) = updateAt 	i 
														({ts!!i & tType = if (left)
															(SEC {sec & sLeftSignal = Just (not (fromJust sec.sLeftSignal))}) 
															(SEC {sec & sRightSignal = Just (not (fromJust sec.sRightSignal))})
														})
														ts
/*
GetTrackIndex
*/

GetTrackIndex_i :: Int [Track] Track -> Int
GetTrackIndex_i _ [] _ = -1
GetTrackIndex_i size [t1:ts] t2 = if(t1.tPosition.xPos == t2.tPosition.xPos && t1.tPosition.yPos == t2.tPosition.yPos) 
									(size - (length ts) - 1) 
									(GetTrackIndex_i size ts t2)

GetTrackIndex :: [Track] Track -> Int
GetTrackIndex ts t = GetTrackIndex_i (length ts) ts t

/*
Get Xs
*/
GetTracksSmallestX :: [Track] -> Int
GetTracksSmallestX t = (GetTracksSmallestX_i t).tPosition.xPos

GetTracksSmallestX_i :: [Track] -> Track
GetTracksSmallestX_i [x] = x
GetTracksSmallestX_i [x:xs] = (\q r -> if (q.tPosition.xPos < r.tPosition.xPos) (q) (r)) x (GetTracksSmallestX_i xs)

GetTracksLargestX :: [Track] -> Int
GetTracksLargestX t = (GetTracksLargestX_i t).tPosition.xPos

GetTracksLargestX_i :: [Track] -> Track
GetTracksLargestX_i [x] = x
GetTracksLargestX_i [x:xs] = (\q r -> if (q.tPosition.xPos > r.tPosition.xPos) (q) (r)) x (GetTracksLargestX_i xs)

/*
Get Ys
*/

CheckSwitchSmall :: Track -> Int
CheckSwitchSmall {tPosition, tType = (SWT {sOrientation=SW})} = tPosition.yPos-1
CheckSwitchSmall {tPosition, tType = (SWT {sOrientation=SE})} = tPosition.yPos-1
CheckSwitchSmall t = t.tPosition.yPos

CheckSwitchLarge :: Track -> Int
CheckSwitchLarge {tPosition, tType = (SWT {sOrientation=NW})} = tPosition.yPos+1
CheckSwitchLarge {tPosition, tType = (SWT {sOrientation=NE})} = tPosition.yPos+1
CheckSwitchLarge t = t.tPosition.yPos

GetTracksSmallestY :: [Track] -> Int
GetTracksSmallestY t = CheckSwitchSmall (GetTracksSmallestY_i t)

GetTracksSmallestY_i :: [Track] -> Track
GetTracksSmallestY_i [x] = x
GetTracksSmallestY_i [x:xs] = (\q r -> if (CheckSwitchSmall(q) < CheckSwitchSmall(r)) (q) (r)) x (GetTracksSmallestY_i xs)

GetTracksLargestY :: [Track] -> Int
GetTracksLargestY t = CheckSwitchLarge (GetTracksLargestY_i t)

GetTracksLargestY_i :: [Track] -> Track
GetTracksLargestY_i [x] = x
GetTracksLargestY_i [x:xs] = (\q r -> if (CheckSwitchLarge(q) > CheckSwitchLarge(r)) (q) (r)) x (GetTracksLargestY_i xs)

/*
Get Size
*/

GetTrackWidth :: [Track] -> Int
GetTrackWidth tracks = (GetTracksLargestX tracks) - ((GetTracksSmallestX tracks)-1)

GetTrackHeight :: [Track] -> Int
GetTrackHeight tracks = (GetTracksLargestY tracks) - ((GetTracksSmallestY tracks)-1)

GetYTracks :: Int [Track] -> [Track]
GetYTracks ypos tracks = filter (\{tPosition={yPos}} -> yPos == ypos) tracks