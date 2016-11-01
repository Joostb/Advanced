definition module types

import iTasks
import iTasks.API.Extensions.SVG.SVGlet

:: Position = { 
		xPos :: Int,
		yPos :: Int
	}	
	
:: Orientation = NE | NW | SE | SW
	
:: Switch = { 
		sOrientation :: Orientation,
		sOn :: Bool
	}	

:: Section = {
		sLeftSignal :: Maybe Bool,
		sRightSignal :: Maybe Bool
	}
	
:: Type = SEC Section | SWT Switch
	
:: Track = {
		tLabel :: String,
		tPosition :: Position,
		tType :: Type
	}

:: Train = {
		position :: Position,
		direction :: Bool,
		inipos :: Position,
		despos :: Position
	}

:: UserRole = Master | Machinist | Controller | Designer

:: UserSettings = {
		uShowGrid :: Bool,
		uShowTrain :: Bool,
		uMoveTrain :: Bool,
		uTrainDriver :: Maybe Train,
		uToggleLights :: Bool,
		uToggleSwitches :: Bool
	}

:: State = {
		tracks :: [Track],
		trains :: [Train]
	}

instance == UserRole
instance == Orientation
instance == Position

GetRoleSettings :: UserRole -> UserSettings

isNorth :: Orientation -> Bool
isEast :: Orientation -> Bool
isSouth :: Orientation -> Bool
isWest :: Orientation -> Bool

isSection :: Track -> Bool
isSwitch :: Track -> Bool

TrainEq :: Train Train -> Bool

ExistsTrackByPosition :: [Track] Position -> Bool
GetTrackByPosition :: [Track] Position -> Track

ToggleSwitch :: [Track] Int -> [Track]
ToggleLight :: [Track] Bool Int -> [Track]

GetTrackIndex :: [Track] Track -> Int

GetTracksSmallestX :: [Track] -> Int
GetTracksSmallestY :: [Track] -> Int
	
GetTrackHeight :: [Track] -> Int
GetTrackWidth :: [Track] -> Int
	

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