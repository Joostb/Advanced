module site

import iTasks
import iTasks.API.Extensions.SVG.SVGlet
import svgDemo

Start :: *World -> *World
Start world = startEngine [publish "/" (WebApp []) (\_ -> task)] world

state :: Shared State 
state = sharedStore "sharedState" {
		tracks = (createTest 4) ++ [{tLabel="s5", tPosition={xPos=4, yPos=0}, tType = (SEC {sLeftSignal = (Just True), sRightSignal = (Just False)})}],
		role = Designer,
		step = 1
	}

task :: Task State
task
	= (		(updateTask <<@ ArrangeHorizontal)
	-||-	(imageTask  <<@ ArrangeHorizontal)) <<@ ArrangeHorizontal

// regular iTask update of shared state
updateTask :: Task State
updateTask = updateSharedInformation (Title "State") [] state

createTestSection :: Int -> Track
createTestSection n = {tLabel="s" +++ toString(n), tPosition={xPos = n, yPos=0}, tType = (SEC {sLeftSignal = (Just True), sRightSignal = (Just False)})}

createTest :: Int -> [Track]
createTest n = map createTestSection [1..n] 

// display the shared state as an image and update it
imageTask :: Task State
imageTask =
	updateSharedInformation
		(Title "Image")
		[imageUpdate
			id										// server state (share) and view are identical
			(\s v tags -> myImage s)				// generate image
			(\s v -> v)								// update view when state changes
			(\s v -> s)								// update state when view changes
			(\_ s -> Nothing)						// no conflict handling
			(\o n.n)								// always select the new state
		]
		state

myImage :: State -> Image State
myImage s = drawTracks s
			  
font = normalFontDef "Arial" 18.0