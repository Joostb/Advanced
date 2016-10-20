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

drawSignal :: (Maybe Bool) -> Image State
drawSignal Nothing = empty zero zero
drawSignal (Just b) = 	overlay 
					[(AtMiddleX, AtMiddleY), (AtMiddleX, AtMiddleY)] 
					[] 
					[circle (px 12.0) <@< {fill = toSVGColor "black"}, circle (px 10.0) <@< {fill = toSVGColor (if (b) ("green") ("red"))}] 
					(Just ((rect (px 25.0) (px 25.0)) <@< {opacity = 0.0} <@< {strokewidth = zero}))
							

sectionBackground :: Image State
sectionBackground = rect sectionWidth sectionHeight <@< {fill = sectionBackgroundColor}


// TODO opcleanen
drawSection :: String Section State -> Image State
drawSection sLabel {sLeftSignal, sRightSignal} _ = overlay 
		[(AtMiddleX, AtBottom), (AtMiddleX, AtMiddleY), (AtLeft, AtTop), (AtRight, AtTop)]
		[] 
		[text font sLabel, drawRail,
		((drawSignal sLeftSignal) 
			<@< {onclick = \i s.{s & tracks = (case s.tracks of
		 											[x:xs] = (case x.tType of
								 							(SEC section) = case section.sLeftSignal of
				 												(Just light) = [{x & tType = (SEC {sLeftSignal = Just (not light), sRightSignal = section.sRightSignal})}:xs]
																Nothing = s.tracks
															(SWT _) = s.tracks)
													[] = [])
											}
				, local = False}), drawSignal sRightSignal] 
		(Just sectionBackground)


drawTracksInternal :: [Track] State -> [Image State]
drawTracksInternal [{tLabel, tType = (SEC s)}:ts] state = [(drawSection tLabel s state):(drawTracksInternal ts state)]
drawTracksInternal [] _ = []
								
drawTracks :: State -> Image State
drawTracks s = beside [] [] (drawTracksInternal s.tracks s) Nothing


