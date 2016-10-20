definition module hoi

import iTasks
import iTasks.API.Extensions.SVG.SVGlet

:: Position = { 
		xPos :: Int,
		yPos :: Int
	}	
	
:: Orientation = NE | NW | SE | SW
	
:: Switch = { 
		sOrientation :: Orientation
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
	
:: State = {
	tracks :: [Track]
	}
	
derive class iTask Position
derive class iTask Orientation

derive class iTask Switch
derive class iTask Section
derive class iTask Type
derive class iTask Track

derive class iTask State