definition module hoi

import iTasks
import iTasks.API.Extensions.SVG.SVGlet

:: Position = { 
		xPos :: Int,
		yPos :: Int
	}
			

:: Section = {
		sLabel :: String,
		sPosition :: Position,
		sLeftSignal :: Maybe Bool,
		sRightSignal :: Maybe Bool
	}

:: UserRole = Machinist | Controller | Designer

:: State = {
		sections :: [Section],
		role :: UserRole,
		step :: Int
	}
	

derive class iTask Position
derive class iTask Section
derive class iTask UserRole
derive class iTask State