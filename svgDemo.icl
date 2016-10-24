implementation module svgDemo

/*
	Pieter Koopman pieter@cs.ru.nl
	Advanced programming, advanced iTask, 2016
	Demo program for SVG and a share
	
	This is an iTask program:
	- use environment iTasks
	- place the executable in the iTasks-SDK folder or a subfolder of it
	- the SVG system can use much memory, for instance 50M (in project options)
*/

import StdArray

import iTasks
import iTasks.API.Extensions.SVG.SVGlet

import hoi

font = normalFontDef "Arial" 14.0

railHeight :: Span
railHeight = px 5.0

railColor :: SVGColor
railColor = toSVGColor "black"

sectionWidth :: Span
sectionWidth = px 100.0

sectionHeight :: Span
sectionHeight = px 100.0

sectionBackgroundColor :: SVGColor
sectionBackgroundColor = toSVGColor "silver"


// Train
wheel :: Image State
wheel = circle (px 30.0)

wheelSpace :: Image State
wheelSpace = empty (px 5.0) (px 5.0)

wheels :: Image State
wheels = beside [] [] [wheel, wheelSpace, wheel, wheelSpace, wheel, wheelSpace, wheel] Nothing

house :: Image State
house = polygon Nothing [(zero,zero),(zero, px 80.0),(px 200.0, px 80.0),(px 100.0, zero)] <@< {fill = toSVGColor "grey"}

drawTrain ::  Bool -> Image State
drawTrain  r = if r flipx id (above (repeat AtMiddleX) [] [house,wheels] Nothing)

// Section
drawRail :: Image State
drawRail = rect sectionWidth railHeight <@< {fill = railColor}					

drawSignal :: Int (Maybe Bool) Bool -> Image State
drawSignal _ Nothing _ = empty zero zero
drawSignal index (Just b) isLeft = overlay 
						[(AtMiddleX, AtMiddleY), (AtMiddleX, AtMiddleY)] 
						[] 
						[circle (px 10.0) 
						<@< {fill = toSVGColor (if (b) ("green") ("red"))}
						<@< {strokewidth = (px 2.5)}
						<@< {onclick = \i s -> {s & tracks =  case (s.tracks!!index).tType of
																(SEC section) = updateAt 	index 
																							({s.tracks!!index & tType = if (isLeft)
																														(SEC {section & sLeftSignal = Just (not b)}) 
																														(SEC {section & sRightSignal = Just (not b)})
																							})
																							s.tracks
												}, local = False}] 
						(Just ((rect (px 25.0) (px 25.0)) <@< {opacity = 0.0} 
						<@< {strokewidth = zero}
						
						))
						
drawLeftSignal :: Int (Maybe Bool) -> Image State
drawLeftSignal arg1 arg2 = drawSignal arg1 arg2 True

drawRightSignal :: Int (Maybe Bool) -> Image State
drawRightSignal arg1 arg2 = drawSignal arg1 arg2 False 
		

sectionBackground :: Image State
sectionBackground = rect sectionWidth sectionHeight <@< {fill = sectionBackgroundColor}

drawSection :: Int Track -> Image State
drawSection index {tLabel, tType = (SEC {sLeftSignal, sRightSignal})} = overlay	
								([(AtMiddleX, AtBottom), (AtMiddleX, AtMiddleY), (AtLeft, AtTop), (AtRight, AtTop)])
								[] 
								([text font tLabel, drawRail, drawLeftSignal index sLeftSignal, drawRightSignal index sRightSignal]) 
								(Just (sectionBackground))
								

drawTrack :: State Int -> Image State
drawTrack s index = case ((s.tracks!!index).tType) of 
						(SEC section) = drawSection index (s.tracks!!index)
								
drawTracks :: State -> Image State
drawTracks s = beside [] [] (map (drawTrack s) [0..((length s.tracks) - 1)]) Nothing


