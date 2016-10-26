implementation module hoi

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

instance == UserRole where
	(==) Designer Designer = True
	(==) Machinist Machinist = True
	(==) Controller Controller = True
	(==) _ _ = False


derive class iTask Position
derive class iTask Orientation

derive class iTask Switch
derive class iTask Section
derive class iTask Type
derive class iTask Track

derive class iTask Train

derive class iTask UserRole

derive class iTask State