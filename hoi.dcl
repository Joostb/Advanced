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