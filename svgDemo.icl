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

:: TrackDraw = EMPTY | SECTION Track | SWITCHA Track | SWITCHB Track

DrawTrackDrawing :: State TrackDraw -> Image State
DrawTrackDrawing _ EMPTY 		= rect sectionWidth sectionHeight <@< {strokewidth = (px 0.0)} <@< {opacity=0.0}
DrawTrackDrawing s (SECTION t) 	= drawSection (GetTrackIndex s.tracks t) t
DrawTrackDrawing _ (SWITCHA t)	= drawSwitchA t
DrawTrackDrawing _ (SWITCHB t)	= drawSwitchB t

BuildEmptyTrackDrawing :: [Track] -> [[TrackDraw]]
BuildEmptyTrackDrawing tracks = repeatn (GetTrackWidth tracks) (repeatn (GetTrackHeight tracks) EMPTY)

BuildTrackDrawing :: [[TrackDraw]] [Track] Int Int -> [[TrackDraw]]
BuildTrackDrawing td [x:xs] offx offy = BuildTrackDrawing (BuildTrackDraw td x (offx) (offy)) xs offx offy
BuildTrackDrawing td [] _ _ = td

BuildTrackDraw :: [[TrackDraw]] Track Int Int-> [[TrackDraw]]
BuildTrackDraw td (t=:{tPosition, tType = (SEC section)}) offx offy 			= updateAt (xpos) (updateAt (ypos) (SECTION t) (td!!xpos)) td
																		where
																			xpos = tPosition.xPos - offx
																			ypos = tPosition.yPos - offy
BuildTrackDraw td (t=:{tPosition, tType = (SWT {sOrientation})}) offx offy = case sOrientation of
																				NE = updateAt xpos (updateAt ypos (SWITCHA t) (newtd!!xpos)) newtd
																				NW = updateAt xpos (updateAt ypos (SWITCHA t) (newtd!!xpos)) newtd
																				SE = updateAt xpos (updateAt ypos (SWITCHA t) (newtd2!!xpos)) newtd2
																				SW = updateAt xpos (updateAt ypos (SWITCHA t) (newtd2!!xpos)) newtd2
																		where
																			newtd = (updateAt xpos (updateAt (ypos+1) (SWITCHB t) (td!!xpos)) td)
																			newtd2 = (updateAt xpos (updateAt (ypos-1) (SWITCHB t) (td!!xpos)) td)
																			xpos = tPosition.xPos - offx
																			ypos = tPosition.yPos - offy
BuildTrackDraw td _ _ _ = td

TrackImageFromDrawing :: State [[TrackDraw]] -> Image State
TrackImageFromDrawing s[x] = above [] [] (reverse (map (DrawTrackDrawing s) x)) Nothing
TrackImageFromDrawing s [x:xs] = beside [] [] [(TrackImageFromDrawing s [x]), TrackImageFromDrawing s xs] Nothing

GetTrackImage :: State -> Image State
GetTrackImage s = TrackImageFromDrawing s (BuildTrackDrawing startgrid s.tracks offx offy)
					where 
						startgrid = BuildEmptyTrackDrawing s.tracks
						offx = GetTracksSmallestX s.tracks
						offy = GetTracksSmallestY s.tracks


font = normalFontDef "Arial" 14.0

railHeight :: Span
railHeight = px 5.0

railColor :: SVGColor
railColor = toSVGColor "black"

sectionWidth :: Span
sectionWidth = px 100.0

sectionHeight :: Span
sectionHeight = px 100.0

switchHeight :: Span
switchHeight = px 200.0

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
drawRail :: Bool -> Image State
drawRail b = xline Nothing sectionWidth <@< {strokewidth = (px 5.0)} <@< {fill = railColor} <@< if(b) ({dash = [5]}) ({dash = []})		

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
								([(AtMiddleX, AtBottom), (AtMiddleX, AtMiddleY), (AtLeft, AtTop), (AtRight, AtTop), (AtMiddleX, AtBottom)])
								[] 
								([text font tLabel, drawRail False, drawLeftSignal index sLeftSignal, drawRightSignal index sRightSignal]) 
								(Just (sectionBackground))
								
drawSwitchRail :: Track Bool -> Image State
drawSwitchRail {tType = (SWT {sOrientation})} b = line Nothing slash (sectionWidth*.0.5) (sectionHeight*.0.5) 
													<@< {fill = railColor} <@< {strokewidth = (px 5.0)} <@< if (b) ({dash = [5]}) ({dash = []})
											where slash = case sOrientation of
															(NW) = Backslash
															(NE) = Slash
															(SW) = Slash
															(SE) = Backslash
								
drawSwitchA :: Track -> Image State
drawSwitchA (t=:{tLabel, tType = (SWT {sOrientation, sOn})}) = overlay	
														[TextPos, (AtMiddleX, AtMiddleY), RailPos]
														[] 
														[text font tLabel, drawRail sOn, drawSwitchRail t (not sOn)]
														(Just (sectionBackground))
														
													where 	RailPos = case sOrientation of
																	(NW) = (AtRight, AtTop)
																	(NE) = (AtLeft, AtTop)
																	(SW) = (AtRight, AtBottom)
																	(SE) = (AtLeft, AtBottom)
															TextPos = case sOrientation of
																	(NW) = (AtMiddleX, AtBottom)
																	(NE) = (AtMiddleX, AtBottom)
																	(SW) = (AtMiddleX, AtTop)
																	(SE) = (AtMiddleX, AtTop)
														
drawSwitchB :: Track -> Image State
drawSwitchB (t=:{tLabel, tType = (SWT {sOrientation, sOn})}) = overlay
														[RailPos]
														[] 
														[drawSwitchRail t (not sOn)]
														(Just (sectionBackground))
													where RailPos = case sOrientation of
																	(NW) = (AtLeft, AtBottom)
																	(NE)  = (AtRight, AtBottom)
																	(SW) = (AtLeft, AtTop)
																	(SE) = (AtRight, AtTop)
								

drawTrack :: State Int -> Image State
drawTrack s index = case ((s.tracks!!index).tType) of 
						(SEC section) = drawSection index (s.tracks!!index)
						//(SWT switch) = drawSwitch index (s.tracks!!index)			

/*drawTracks :: State -> Image State
drawTracks s = beside 
				[]
				[]
				(map (drawTrack s) [0..((length s.tracks) - 1)])
				Nothing
*/
drawTracks :: State -> Image State
drawTracks s = GetTrackImage s

