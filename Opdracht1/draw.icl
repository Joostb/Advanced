implementation module draw

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

import types

:: TrackDraw = EMPTY | SECTION Track | SWITCHA Track | SWITCHB Track

DrawTrackDrawing :: State UserSettings TrackDraw -> Image State
DrawTrackDrawing _ _ EMPTY 		= rect sectionWidth sectionHeight <@< {strokewidth = (px 0.0)} <@< {opacity=0.0}
DrawTrackDrawing s us (SECTION t) 	= overlay [] [(px 22.0, px 15.0)] (TrackHasTrain s us t) (Just (drawSection us (GetTrackIndex s.tracks t) t))

DrawTrackDrawing s us (SWITCHA (t=:{tType = (SWT {sOn, sOrientation})})) 
	= if(not sOn)
		(overlay [] [(px 22.0, px 15.0)] (TrackHasTrain s us t) (Just (drawSwitchA us t)))
		(if(isNorth sOrientation)
			(drawSwitchA us t)
			(overlay [(AtLeft, AtBottom)] [pos] (map (rotate (deg d)) (TrackHasTrain s us t)) (Just (drawSwitchA us t))))
	where (d,pos) = case sOrientation of
						SE = ((45.0), (px -35.0, px 42.0))
						SW = ((-45.0), (px 35.0, px -63.0))

DrawTrackDrawing s us (SWITCHB (t=:{tType = (SWT {sOn, sOrientation})})) 
	= if(not sOn || isSouth sOrientation) 
		(drawSwitchB us t)
		(overlay [(AtLeft, AtBottom)] [pos] (map (rotate (deg d)) (TrackHasTrain s us t)) (Just (drawSwitchB us t)))
	where (d, pos) = case sOrientation of 
						NE = ((-45.0), (px 35.0, px -65.0))
						NW = ((45.0), (px -35.0, px 40.0))
										

TrackHasTrain :: State UserSettings Track -> [Image State]
TrackHasTrain s {uShowTrain, uTrainDriver = Just train} t
	= if (uShowTrain && (length trains) > 0) 
		[(fitx (px 60.0) (drawTrain (trains!!0).Train.direction)) <@< if((length movable) > 0) {onclick = \i s -> drive (movable!!0) s, local=False} {onclick = \i s -> s, local=False}] 
		[]
	where 	trains = filter (\{position}. (t.tPosition == position)) s.trains
			movable = filter (\train2 . TrainEq train train2) trains
			
TrackHasTrain s {uShowTrain, uMoveTrain} t
	= if (uShowTrain && (length trains) > 0) 
		[(fitx (px 60.0) (drawTrain (trains!!0).Train.direction)) <@< if(uMoveTrain) {onclick = \i s -> drive (trains!!0) s, local=False} {onclick = \i s -> s, local=False}] 
		[]
	where trains = (filter (\{position}. (t.tPosition == position)) s.trains)


drive :: Train State -> State
drive t s = {s & trains = tr}
			where tr = map 
						(\(train=:{position}). if (TrainEq train t && t.position == position)
									(driveCheck train s)
									(train))
						s.trains
									
driveCheck :: Train State -> Train
driveCheck (t=:{direction, position={xPos, yPos}}) s = if (not (ExistsTrackByPosition s.tracks {xPos=nextX, yPos=nextY})) 
														{t & Train.direction = (not direction)}
														(case nextTrack of
															{tType = (SEC section)} = ({t & position={xPos=nextX, yPos=nextY}}) //Next track is section -> go there
															{tType = (SWT switch)} = driveSwitch t nextTrack s					//Next track is switch -> we have a problem
														)
			where 	nextX = xPos + (if direction (-1) 1)
					nextY = yPos + currentSwitch
					nextTrack = GetTrackByPosition s.tracks {xPos=nextX, yPos=nextY}
					currentSwitch = case (GetTrackByPosition s.tracks {xPos=xPos, yPos=yPos}).tType of 
									(SEC s)     = 0
									(SWT swtch) = 	if ((not swtch.sOn) || (direction && (isEast swtch.sOrientation)) || ((not direction) && (isWest swtch.sOrientation)))
													0
													if (isNorth swtch.sOrientation) (1) (-1)
					
driveSwitch :: Train Track State -> Train
driveSwitch (train=:{direction, position={xPos, yPos}}) (track=:{tPosition={xPos=nextX, yPos=nextY}, tType=(SWT {sOrientation, sOn})}) s
			|yPos < nextY 	= if ((sOn && isSouth sOrientation) || (not sOn && isNorth sOrientation)) (trainGoes) (trainStays)
			|yPos > nextY 	= if ((sOn && isNorth sOrientation) || (not sOn && isSouth sOrientation)) (trainGoes) (trainStays)
			|yPos == nextY	= if (sOn && ((xPos < nextX && isWest sOrientation) || (xPos > nextX && isEast sOrientation))) (trainStays) (trainGoes)
		where
			trainGoes = ({train & position={xPos=nextX, yPos=nextY}})
			trainStays = train



BuildEmptyTrackDrawing :: [Track] -> [[TrackDraw]]
BuildEmptyTrackDrawing tracks = repeatn (GetTrackWidth tracks) (repeatn (GetTrackHeight tracks) EMPTY)

BuildTrackDrawing :: [[TrackDraw]] [Track] Int Int -> [[TrackDraw]]
BuildTrackDrawing td [x:xs] offx offy = BuildTrackDrawing (BuildTrackDraw td x (offx) (offy)) xs offx offy
BuildTrackDrawing td [] _ _ = td

BuildTrackDraw :: [[TrackDraw]] Track Int Int-> [[TrackDraw]]
BuildTrackDraw td (t=:{tPosition, tType = (SEC section)}) offx offy = updateAt (xpos) (updateAt (ypos) (SECTION t) (td!!xpos)) td
																		where
																			xpos = tPosition.xPos - offx
																			ypos = tPosition.yPos - offy
BuildTrackDraw td (t=:{tPosition, tType = (SWT {sOrientation})}) offx offy = if(isNorth sOrientation) 
																				(updateAt xpos (updateAt ypos (SWITCHA t) (newtd!!xpos)) newtd) 
																				(updateAt xpos (updateAt ypos (SWITCHA t) (newtd2!!xpos)) newtd2)
																		where
																			newtd = (updateAt xpos (updateAt (ypos+1) (SWITCHB t) (td!!xpos)) td)
																			newtd2 = (updateAt xpos (updateAt (ypos-1) (SWITCHB t) (td!!xpos)) td)
																			xpos = tPosition.xPos - offx
																			ypos = tPosition.yPos - offy
BuildTrackDraw td _ _ _ = td

TrackImageFromDrawing :: State UserSettings [[TrackDraw]] -> Image State
TrackImageFromDrawing s us [x] = above [] [] (reverse (map (DrawTrackDrawing s us) x)) Nothing
TrackImageFromDrawing s us [x:xs] = beside [] [] [(TrackImageFromDrawing s us [x]), TrackImageFromDrawing s us xs] Nothing

GetTrackImage :: State UserSettings -> Image State
GetTrackImage s us = TrackImageFromDrawing s us (BuildTrackDrawing startgrid s.tracks offx offy)
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

sectionDestinationColor :: SVGColor
sectionDestinationColor = toSVGColor "maroon"


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

drawSignal :: UserSettings Int (Maybe Bool) Bool -> Image State
drawSignal _ _ Nothing _ = empty zero zero
drawSignal {uToggleLights} index (Just b) isLeft = overlay 
						[(AtMiddleX, AtMiddleY), (AtMiddleX, AtMiddleY)] 
						[] 
						[circle (px 10.0) 
						<@< {fill = toSVGColor (if (b) ("green") ("red"))}
						<@< {strokewidth = (px 2.5)}
						<@< if(uToggleLights) {onclick = \i s -> {s & tracks = ToggleLight s.tracks isLeft index}, local = False} {onclick = \i s -> s, local = False}] 
						(Just ((rect (px 25.0) (px 25.0)) <@< {opacity = 0.0} 
						<@< {strokewidth = zero}
						
						))
						
drawLeftSignal :: UserSettings Int (Maybe Bool) -> Image State
drawLeftSignal us arg1 arg2 = drawSignal us arg1 arg2 True

drawRightSignal :: UserSettings Int (Maybe Bool) -> Image State
drawRightSignal us arg1 arg2 = drawSignal us arg1 arg2 False 
		

sectionBackground :: UserSettings Track -> Image State
sectionBackground {uTrainDriver = Nothing} _ = rect sectionWidth sectionHeight <@< {fill = sectionBackgroundColor}
sectionBackground {uTrainDriver = Just train} {tPosition} = rect sectionWidth sectionHeight <@< {fill = if(train.despos == tPosition) sectionDestinationColor sectionBackgroundColor}

drawSection :: UserSettings Int Track -> Image State
drawSection us index (t=:{tLabel, tType = (SEC {sLeftSignal, sRightSignal})}) = overlay	
								([(AtMiddleX, AtBottom), (AtMiddleX, AtMiddleY), (AtLeft, AtTop), (AtRight, AtTop), (AtMiddleX, AtBottom)])
								[] 
								([text font tLabel, drawRail False, drawLeftSignal us index sLeftSignal, drawRightSignal us index sRightSignal]) 
								(Just (sectionBackground us t))
								
drawSwitchRail :: Track Bool -> Image State
drawSwitchRail {tType = (SWT {sOrientation})} b = line Nothing slash (sectionWidth*.0.5) (sectionHeight*.0.5) 
													<@< {fill = railColor} <@< {strokewidth = (px 5.0)} <@< if (b) ({dash = [5]}) ({dash = []})
											where slash = case sOrientation of
															(NW) = Backslash
															(NE) = Slash
															(SW) = Slash
															(SE) = Backslash
								
drawSwitchA :: UserSettings Track -> Image State
drawSwitchA (us=:{uToggleSwitches}) (t=:{tLabel, tType = (SWT {sOrientation, sOn})}) 
	= 	overlay	
			[TextPos, (AtMiddleX, AtMiddleY), RailPos]
			[] 
			[text font tLabel, drawRail sOn, drawSwitchRail t (not sOn)]
			(Just (sectionBackground us t))
		<@< if(uToggleSwitches) {onclick= \i s -> {s & tracks = ToggleSwitch s.tracks (GetTrackIndex s.tracks t)}, local=False} {onclick = \i s -> s, local=False}
														
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
														
drawSwitchB :: UserSettings Track -> Image State
drawSwitchB (us=:{uToggleSwitches}) (t=:{tLabel, tType = (SWT {sOrientation, sOn})}) 
	= 	overlay
			[RailPos]
			[] 
			[drawSwitchRail t (not sOn)]
			(Just (sectionBackground us t))
		<@< if(uToggleSwitches) {onclick= \i s -> {s & tracks = ToggleSwitch s.tracks (GetTrackIndex s.tracks t)}, local=False} {onclick = \i s -> s, local=False}
	where RailPos = case sOrientation of
						(NW) = (AtLeft, AtBottom)
						(NE)  = (AtRight, AtBottom)
						(SW) = (AtLeft, AtTop)
						(SE) = (AtRight, AtTop)
								

drawTracks :: State UserSettings -> Image State
drawTracks s us = GetTrackImage s us

