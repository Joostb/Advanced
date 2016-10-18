module site

import iTasks
import iTasks.API.Extensions.SVG.SVGlet
import svgDemo

Start :: *World -> *World
Start world = startEngine [publish "/" (WebApp []) (\_ -> task)] world

:: State = {red :: Bool, clicks :: Int}
derive class iTask State

state :: Shared State
state = sharedStore "sharedState" {red = False, clicks = 0}

task :: Task State
task
	= (		(updateTask <<@ ArrangeHorizontal)
	-||-	(imageTask  <<@ ArrangeHorizontal)) <<@ ArrangeHorizontal

// regular iTask update of shared state
updateTask :: Task State
updateTask = updateSharedInformation (Title "State") [] state

// display the shared state as an image and update it
imageTask :: Task State
imageTask =
	updateSharedInformation
		(Title "Image")
		[imageUpdate
			id												// server state (share) and view are identical
			(\s v tags -> myImage s)	// generate image
			(\s v -> v)								// update view when state changes
			(\s v -> s)								// update state when view changes
			(\_ s -> Nothing)					// no conflict handling
			(\o n.n)									// always select the new state
		]
		state

myImage :: State -> Image State
myImage s = 
	overlay [(AtMiddleX,AtMiddleY)] [] [text font (toString s.clicks)]
	(Just (train False
					<@< {fill = toSVGColor (if s.red "orangered" "lightgreen")}
					<@< {onclick = \i s.{s & clicks = i + s.clicks, red = not s.red}, local = False}
			  ))

font = normalFontDef "Arial" 18.0