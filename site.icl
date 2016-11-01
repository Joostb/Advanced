module site

import iTasks
import iTasks.API.Extensions.SVG.SVGlet
import draw

CreateStartTracks :: [Track]
CreateStartTracks = [{tLabel="s11", tPosition={xPos=1, yPos=1}, tType = (SEC {sLeftSignal = Nothing, sRightSignal = Nothing})},
					{tLabel="s12", tPosition={xPos=2, yPos=1}, tType = (SEC {sLeftSignal = Nothing, sRightSignal = (Just True)})},
					{tLabel="s13", tPosition={xPos=3, yPos=1}, tType = (SWT {sOrientation = NW, sOn = False})},
					{tLabel="s14", tPosition={xPos=4, yPos=1}, tType = (SEC {sLeftSignal = (Just True), sRightSignal = (Just True)})},
					{tLabel="s15", tPosition={xPos=5, yPos=1}, tType = (SWT {sOrientation = NE, sOn = False})},
					{tLabel="s16", tPosition={xPos=6, yPos=1}, tType = (SEC {sLeftSignal = (Just True), sRightSignal = Nothing})},
					{tLabel="s17", tPosition={xPos=7, yPos=1}, tType = (SEC {sLeftSignal = Nothing, sRightSignal = Nothing})},
					
					{tLabel="s21", tPosition={xPos=1, yPos=2}, tType = (SEC {sLeftSignal = Nothing, sRightSignal = Nothing})},
					{tLabel="s22", tPosition={xPos=2, yPos=2}, tType = (SEC {sLeftSignal = Nothing, sRightSignal = Nothing})},
					{tLabel="s26", tPosition={xPos=6, yPos=2}, tType = (SEC {sLeftSignal = Nothing, sRightSignal = Nothing})},
					{tLabel="s27", tPosition={xPos=7, yPos=2}, tType = (SEC {sLeftSignal = Nothing, sRightSignal = Nothing})}
					]
					
CreateStartTrains :: [Train]
CreateStartTrains = [{inipos={xPos=1, yPos=1}, despos={xPos=7, yPos=1}, position={xPos=1,yPos=1}, direction=True},
					{inipos={xPos=7, yPos=1}, despos={xPos=1, yPos=1}, position={xPos=1, yPos=1}, direction=False}]

Start :: *World -> *World
Start world = startEngine [publish "/" (WebApp []) (\_ -> task)] world

state :: Shared State
state = sharedStore "sharedState" {
			tracks = CreateStartTracks,
			trains = CreateStartTrains
		}

task :: Task State
task
	=  doIdentified >>= \role . nextTask role  


// TODO Verschillende tasks implementeren
nextTask :: UserRole -> Task State 
nextTask role
	| role==Master		= masterTask (GetRoleSettings role)
	| role==Designer  	= designerTask (GetRoleSettings role)
	| role==Machinist 	= machinistTask (GetRoleSettings role)
    | role==Controller 	= controllerTask (GetRoleSettings role)
    
/**
	Master Task
**/

masterTask :: UserSettings -> Task State
masterTask us = ((updateTask <<@ ArrangeHorizontal)
							-||-	(imageTask "Track" us <<@ ArrangeHorizontal) <<@ ArrangeHorizontal)
							
/**
	Designer Task
**/

designerTask :: UserSettings -> Task State
designerTask us = ((updateTask <<@ ArrangeHorizontal)
							-||-	(imageTask "Track" us <<@ ArrangeHorizontal) <<@ ArrangeHorizontal)

/** 
	Machinist Task
**/

machinistTask :: UserSettings -> Task State
machinistTask settings = (get state) 
							>>= \s . enterChoice "Select your train" [] s.trains 
							>>= \tr . upd (\st -> {st & trains= map (\t -> if(TrainEq tr t) ({t & position = t.inipos}) t) st.trains}) state
							>>|  imageTask "Track" {settings & uTrainDriver = Just tr}

/**
Controller Task 
**/
controllerTask :: UserSettings -> Task State
controllerTask us = ((controllerView <<@ ArrangeHorizontal) ||- (imageTask "Controller View" us <<@ ArrangeHorizontal))<<@ ArrangeHorizontal

updateLights :: [Track] -> [Task State]
updateLights ts = flatten (map (\(a, b) -> updateLight a b) (zip2 ts [0..((length ts) - 1)]))

updateLight :: Track Int -> [Task State]
updateLight {tType = (SWT _)} n = []
updateLight {tLabel, tType = (SEC {sLeftSignal, sRightSignal})} n 
	= (case sLeftSignal of 
			Nothing = []
			(Just _) = [updateSharedInformation 
							(tLabel +++ " Left Signal")
							[UpdateWith 
								(\s -> case (s.tracks!!n).tType of (SEC sec) = fromJust sec.sLeftSignal)
								(\s ls -> case (s.tracks!!n).tType of (SEC sec) = {s & tracks = (updateAt n {(s.tracks!!n) & tType = (SEC {sec & sLeftSignal = Just (ls)})} s.tracks)})
							] state])
		++ case sRightSignal of 
				Nothing = []
				(Just _) = [updateSharedInformation 
								(tLabel +++ " Right Signal")
								[UpdateWith 
									(\s -> case (s.tracks!!n).tType of (SEC sec) = fromJust sec.sRightSignal)
									(\s ls -> case (s.tracks!!n).tType of (SEC sec) = {s & tracks = (updateAt n {(s.tracks!!n) & tType = (SEC {sec & sRightSignal = Just (ls)})} s.tracks)})
							] state]
										
controllerView :: Task State
controllerView = (get state) >>= \s . foldl (-||-) (viewSharedInformation "Controller Panel" [ViewWith (\s -> "")] state) ((updateLights s.tracks) ++ (map trainControllerView [0..((length s.trains)-1)]))

trainControllerView :: Int -> Task State
trainControllerView n = updateSharedInformation ("Train " +++ toString(n) +++ " Initial Position")
											[UpdateWith 
											(\s -> (s.trains!!n).inipos)
											(\s ipos -> {s & trains=(updateAt n {s.trains!!n & inipos = ipos} s.trains)})]
											state
						-||- updateSharedInformation ("Train " +++ toString(n) +++ " Destination")
											[UpdateWith 
											(\s -> (s.trains!!n).despos)
											(\s ipos -> {s & trains=(updateAt n {s.trains!!n & despos = ipos} s.trains)})]
											state


doIdentified :: Task UserRole 
doIdentified  = enterInformation "Enter your role" []

// regular iTask update of shared state
updateTask :: Task State
updateTask = updateSharedInformation (Title "State") [] state

updateTask2 :: [Track] -> Task State
updateTask2 ts = upd (\s -> {s & tracks = ts}) state

createTestSection :: Int -> Track
createTestSection n = {tLabel="s" +++ toString(n), tPosition={xPos = n, yPos=0}, tType = (SEC {sLeftSignal = (Just True), sRightSignal = (Just False)})}

createTest :: Int -> [Track]
createTest n = map createTestSection [1..n] 

myImage :: UserSettings State -> Image State
myImage settings s = drawTracks s settings
			  
font = normalFontDef "Arial" 18.0

// display the shared state as an image and update it
imageTask :: String UserSettings -> Task State
imageTask name settings  =
	updateSharedInformation
		(Title name)
		[imageUpdate
			id										// server state (share) and view are identical
			(\s v tags -> myImage settings s)				// generate image
			(\s v -> v)								// update view when state changes
			(\s v -> s)								// update state when view changes
			(\_ s -> Nothing)						// no conflict handling
			(\o n.n)								// always select the new state
		]
		state