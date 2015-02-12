{-# LANGUAGE OverloadedStrings #-}
--import Criterion.Measurement
import Graphics.Blank
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
--import qualified Graphics.Blank.Style as Gradient
import qualified Data.Text as Text
import qualified Graphics.Blank.Style as Style
import Data.List

type Value = Double
type Angle = Double 
data Key = LeftKey | RightKey | PauseKey | NoKey
			deriving (Show)

data BallKey = MouseUp | MouseLeft | MouseRight | MouseDown  | No
			deriving (Show)	
			
data Intersection =   Horizontal | Vertical | NoIntersection | Wrong	
			deriving (Show)


data GameState = GameState {
					getBall::Ball
					,getPaddle::Paddle
					,getBricks:[Bricks]
				}

data Ball = Ball {
					ballPoint::(Double,Double)
					,ballRadius::Double
				} 			
data Paddle = Paddle {
					paddlePoint::(Double,Double) 
					,currentDimension::(Double,Double)
				}
data Bricks = Bricks	{
					isBrickDestroyable::Bool
					,getBrickDimension::Rect
					,getBrickGradient::Color	
				}
type Rect = (Double,Double,Double,Double)				
defaultPaddlePoint=(0.00,0.93)
-- Step Size 
stepSize = 0.025 
--Paddle Dimensions	as a Factor
breadthPaddle = 0.02
defaultPaddleDimension = (5 * stepSize, breadthPaddle)

defaultPaddleColor = "lightgreen"
defaultBallColor = "blue" 


defaultBallPoint = (0.03,0.93)
defaultballRadius = (0.005,0.005) -- as a percentage of height and width 
defaultBallVector = (0.001,0.003)

					
main = blankCanvas 3000 {events= ["keydown"]} $ \context -> do 	
				lock <- newMVar (NoKey,No)  
				let (w,h) = (width context,height context)
				forkIO (actionListenerBallPaddle context lock)
				loop context lock  GameState {
								getPaddle= Paddle	{ 
									 	paddlePoint = multiplytuple defaultPaddlePoint [w,h]
										,currentDimension = multiplytuple defaultPaddleDimension	[w,h]
									}
								,getBall = Ball {
										 ballPoint = multiplytuple defaultBallPoint [w,h] 
										,ballRadius = calcRadius defaultballRadius [w,h]
									}
								}	
				
				
				
loop::DeviceContext -> MVar (Key,BallKey) -> GameState -> IO ()
loop context lock  gameState = do				
			-- get the gameState
				let paddle = getPaddle gameState
				let ball = getBall gameState
				let (w,h) = (width context,height context)
			-- get Paddle 
				let (px1,py) = paddlePoint paddle
				let (pLength,pBreadth) = currentDimension paddle
				let pdx = 0.025 * w 
				
			-- get Ball			 
				let (bx1,by1) = ballPoint ball
				let ballRad = ballRadius ball
				let (bdx,bdy) = (multiplytuple defaultBallVector [w,h])													
				
				key <- takeMVar lock
				putMVar lock  (NoKey,No) 
				let (keyPaddle,keyBall) = key 
				
				
				let px = case keyPaddle of 
							LeftKey -> if px1-pdx >= 0 then px1-pdx
														else px1
							RightKey -> if px1+ pdx + pLength <= w then px1 + pdx 
					 								else px1
							otherwise -> px1 
				
				let (bx,by) = case keyBall of 
								MouseUp	   -> (bx1,by1 - bdy) 
								MouseDown   -> (bx1,by1 + bdy)
								MouseLeft   -> (bx1 -bdx ,by1) 
								MouseRight  -> (bx1 +bdx ,by1)
								otherwise -> (bx1,by1)
								
				(debugIntersection,angle)<- debugfindIntersectionAxis context (px,py) (pLength,pBreadth) (bx,by,ballRad)				
				--let (debugIntersection,angle) =  findIntersectionAxis  (px,py) (pLength,pBreadth) (bx,by,ballRad)				
				
				let circleColor = case debugIntersection of
										 (NoIntersection) -> "green"
										 (Horizontal)-> "blue"
										 (Vertical)-> "red"
				{-
				if (bx,by) /= (bx1,by1) then 
					(do 
						print angle)
				else 		 
					(return ())	
				-}				 
				--let circleColor = if intersectionRectCircle (px,py) (pLength,pBreadth) (bx,by,ballRad)				
				--					then "red"
				--					else "lightgreen"
				
									--Horizontal -> "blue"
									--Vertical -> "red"
									--NoIntersection -> "lightgreen"
				
				
				
				-- Paddle Object
				--print ("(Width,Height) " ++ (show (w,h)))		
				--print ("Pdx  " ++ (show pdx))
				--print ("PaddleDimension "++ (show (pLength,pBreadth)))
				--print ("Point Paddle " ++ (show (px,py)))
					
				-- Ball Object
				--print ("BallPosition " ++ (show (bx,by)))
				--print ("BallVector "++ (show (bdx,bdy)))
				--print ("BallRadius "++ (show ballRad) )
				
				
				send context (do
						clearRect (0,0,w,h)
						fillStyle defaultPaddleColor
						drawPaddle (px,py) (pLength,pBreadth)
						fillStyle circleColor
						drawBall (bx,by) ballRad
					)
					
				-- Paddle Obj	
				let returnPaddle = Paddle 	{
											 paddlePoint = (px,py)
											,currentDimension = (pLength,pBreadth)
										}
				-- ball Obj	
				let returnBall = Ball 	{
											 ballPoint = (bx,by)
											,ballRadius = ballRad	
										}
										
				-- final return objec	
				let returnGameState = GameState {
													getPaddle=returnPaddle
													,getBall=returnBall
											}																
					
				threadDelay (20 * 20)
				loop context lock returnGameState

--------------------
---    Rect 1 
---
---
---
---		x1	--------------------   x2 , y1  
---			|                   |
---			|                   |
---			|                   |
---			|                   |
---			|                   |
---			|                   |
---			|                   |
---         --------------------- 		y2
---
---
---
---		Rect 2	
---
---								
--		   a--------------------- b,alpha   
---			|                   |
---			|                   |
---			|                   |
---		    |       	        | 
---			|                   |
---			|                   |
---			|                   |
---         --------------------- beta 		
---  `							
---		Condition For Intersection 
---			i.		a<x2
---         ii.		b>x1
---			iii.	alpha<y2
---			iv.		beta>y1
---
--- 
-------------------
---        Rect 1 
---
---
---
---		x1	--------------------   x2 , y1  
---			|                   |
---			|                   |
---			|                   |
---			|                   |
---			|                   |
---			|                   |
---			|                   |
---         --------------------- 		y2
---
---        
---
---        
---       Circle 
---        
---
---					alpha 
---			--------------------   
---			|         |         |
---			|      	  |         |
---			|         |         |
---		   a|------(xr,yr)------| b
---			|         |         |
---			|         |         |
---			|         |         |
---         --------------------- 		
---  `				beta
---        
---		Condition For Intersection 
---			i.		a<x2
---         ii.		b>x1
---			iii.	alpha<y2
---			iv.		beta>y1
---
---
---
-------------------
debugIntersectionRectRect :: DeviceContext -> (Double,Double) -> (Double,Double) -> (Double,Double) -> (Double,Double) -> IO Bool
debugIntersectionRectRect context (x1,y1) (length1,breadth1) (a,alpha) (length2,breadth2) = 
																		do
																		i <- return (intersectionRectRect (x1,y1) (length1,breadth1) (a,alpha) (length2,breadth2))	
																		send context (do
																			putOnScreen ["cond1 -> "] (300,100)
																			putOnScreen ["cond2 -> "] (300,200)
																			putOnScreen ["cond3 -> "] (300,300)
																			putOnScreen ["cond4 -> "] (300,400)
																			putOnScreen [show cond1] (450,100)
																			putOnScreen [show cond2] (450,200)
																			putOnScreen [show cond3] (450,300)
																			putOnScreen [show cond4] (450,400)
																			beginPath()
																			rect (a,alpha,length2,breadth2)
																			closePath ()
																			stroke ()
																			putOnScreen [show i] (550,250)
																			return i
																			)
																		where
																	 		x2 = x1 + length1
																	 		y2 = y1 + breadth1
																	 		b = a + length2
																	 		beta = alpha + breadth2
																			cond1 = a < x2
																	 		cond2 = b > x1 
																			cond3 = alpha  < y2
																			cond4 = beta > y1
																			
																			
debugIntersectionRectCircle :: DeviceContext -> (Double,Double) -> (Double,Double) -> (Double,Double,Double)  -> IO Bool
debugIntersectionRectCircle context (x1,y1) (l,b)  (xc,yc,r) = debugIntersectionRectRect context (x1,y1) (l,b) (xc-r,yc-r) (2 * r,2 *r) 

		 	
debugfindIntersectionAxis ::DeviceContext ->  (Double,Double) -> (Double,Double) -> (Double,Double,Double)  -> IO (Intersection,Value)  
debugfindIntersectionAxis context (xp,yp) (l,b)  (xc,yc,r) = do 
																intersects context
																where
																(lr,br) = (2 * r,2 *r)
																(xr,yr) = (xc -r , yc-r)
																paddleXMax = xp + l 
																paddleYMax = yp + b
																paddleMid = xp + l/2
																hLine (xh,yh) = if xh <= paddleXMax
																					then (xh,yh):hLine (xh + step,yh)
																				  	else []
																vLine (xv,yv) = if yv <= paddleYMax
																					then (xv,yv):vLine (xv,yv+step)
																				  	else []
					 											hLineTop = hLine (xp,yp)
																hLineBottom = hLine (xp,paddleYMax)
																vLineLeft = vLine (xp,yp)
																vLineRight = vLine (paddleXMax,yp) 				   
																intersects context =
																					(
																						do 
																						if intersectionRectRect (xp,yp) (l,b) (xr,yr) (lr,br)
																						then 
																							calculateIntersection context 
																						else
																							return ((NoIntersection,0))
																					)
																calculateIntersection context = 
																		(
																		do
																			send context ( do
																				save ()
																				scale (2,2)
																				putOnScreen ["Paddle x,y ",show (xp,yp)] (10,10)
																				putOnScreen ["Paddle Dimension ",show (l,b)] (10,25)
																				putOnScreen ["Paddle Bottom ",show (paddleXMax,paddleYMax)] (10,40)
																				putOnScreen ["Ball Dimensions ",show (xc,yc,r)](10,55)  	
																				putOnScreen ["IntersectionArray ",show tempList] (10,70) 
																				putOnScreen ["debugMin,minimumIndex) ",show (minPt,minRef)] (10,85)
																				putOnScreen ["debugMax,maximumIndex) ",show (maxPt,maxRef)] (10,100)
																				
																				putOnScreen ["Intersection",show (intersection,midPt)] (10,115)																				
																				putOnScreen ["MidptBallIntersection ",show midPt]  (10,130)
																				putOnScreen ["paddleMid ",show paddleMid]   (10,145)																				
																				putOnScreen ["midPt-paddleMid ",show (midPt - paddleMid)] (10,160)
																				putOnScreen ["Angle ",show ((midPt-paddleMid )/l)] (10,175)
																				restore ()
																				case intersection of
																					  Horizontal -> (
																					  				do 
																						  			beginPath ()
																									strokeStyle "green"
																									moveTo (paddleMid,yp)
																									lineTo (paddleMid,yp - 30)
																									closePath () 
																									stroke ()
																						  			beginPath ()
																									strokeStyle "red"
																									moveTo (midPt,yp)
																									lineTo (midPt,yp - 30)
																									closePath () 
																									stroke ()

																									)				
																					  otherwise ->  return ()		
																				)	 
																			return (intersection,(midPt-paddleMid )/l) 
																		)						 
																			where
																				boolHLineTop = fmap (insideCircle) hLineTop
																				boolHLineBottom = fmap (insideCircle)hLineBottom
																				boolVLineLeft = fmap (insideCircle) vLineLeft
																				boolVLineRight = fmap (insideCircle) vLineRight																
																
																				countHLineTop = noIntersections boolHLineTop 0
																				countHLineBottom = noIntersections boolHLineBottom 0
																				countVLineLeft = noIntersections boolVLineLeft 0
																				countVLineRight = noIntersections boolVLineRight 0
																
																				tempList = [countHLineTop,countHLineBottom,countVLineLeft,countVLineRight]
																				maxNoIntersections = maximum [countHLineTop,countHLineBottom,countVLineLeft,countVLineRight]
																				element = elemIndex maxNoIntersections tempList
																				(returnLine,returnBool,maybeIntersection,ref)= case element of
																							  		Just 0 -> (hLineTop,boolHLineTop,Horizontal,1)
																									Just 1 -> (hLineBottom,boolHLineBottom,Horizontal,2)
																									Just 2 -> (vLineLeft,boolVLineLeft,Vertical,3)
																									Just 3 -> (vLineRight,boolVLineRight,Vertical,4)
																				intersection = if maxNoIntersections >= 2 then maybeIntersection
																					   			else NoIntersection
																								
																				midPt = case intersection of
																							Horizontal -> computeMidPt returnLine returnBool 
																							otherwise -> 0 
																				(_,(minPt,minRef,maxPt,maxRef)) = (debugcomputeMidPt returnLine returnBool)			
																				computeMidPt pts bools = fst (debugcomputeMidPt pts bools)
																				debugcomputeMidPt pts bools = (findTupleMidPt minPt maxPt,(minPt,minRef,maxPt,maxRef))															  
																												where
																													(minPt,minRef) = minIntersection pts bools 0
																													minIntersection (p:ps) (b:bs) minIndex =
																														if b==True then (p,minIndex)
																															else minIntersection ps bs (minIndex+1) 
																													minIntersection [] [] minIndex = (pts !! (minIndex-1),minIndex)
																													(maxPt,maxRef) = maxIntersection (drop minRef pts) (drop minRef bools) minRef 
																													maxIntersection (p:ps) (b:bs) maxIndex =  
																														if b==False then (p,maxIndex)
																															else maxIntersection ps bs (maxIndex+1)  
																													maxIntersection [] [] maxIndex = (pts !! (maxIndex-1),maxIndex)
																													findTupleMidPt (x1,y1) (x2,y2) = (x1+x2)/2 	
																				noIntersections (x:xs) count = if x == True 
																													then noIntersections xs (count +1) 
																													else noIntersections xs count   									
																				noIntersections [] count = count	   						
																insideCircle (x,y) = if (x -xc)^2 + (y-yc)^2 <= r^2 then True else False			
																step = 1 / 5 																								
																		
																	
																	
findIntersectionAxis:: (Double,Double) -> (Double,Double) -> (Double,Double,Double)  -> (Intersection,Value)  
findIntersectionAxis (xp,yp) (l,b)  (xc,yc,r)  = intersects 
												where
												(lr,br) = (2 * r,2 *r)
												(xr,yr) = (xc -r , yc-r)
												paddleXMax = xp + l 
												paddleYMax = yp + b
												paddleMid = xp + l/2
												hLine (xh,yh) = if xh <= paddleXMax
																	then (xh,yh):hLine (xh + step,yh)
																  	else []
												vLine (xv,yv) = if yv <= paddleYMax
																	then (xv,yv):vLine (xv,yv+step)
																  	else []
	 											hLineTop = hLine (xp,yp)
												hLineBottom = hLine (xp,paddleYMax)
												vLineLeft = vLine (xp,yp)
												vLineRight = vLine (paddleXMax,yp) 				   
												intersects = if intersectionRectRect (xp,yp) (l,b) (xr,yr) (lr,br)
																then 
																	calculateIntersection 
																else
																	(NoIntersection,0)
												calculateIntersection =  (intersection,midPt)
															where
																boolHLineTop = fmap (insideCircle) hLineTop
																boolHLineBottom = fmap (insideCircle)hLineBottom
																boolVLineLeft = fmap (insideCircle) vLineLeft
																boolVLineRight = fmap (insideCircle) vLineRight																
																
																countHLineTop = noIntersections boolHLineTop 0
																countHLineBottom = noIntersections boolHLineBottom 0
																countVLineLeft = noIntersections boolVLineLeft 0
																countVLineRight = noIntersections boolVLineRight 0
																
																tempList = [countHLineTop,countHLineBottom,countVLineLeft,countVLineRight]
																maxNoIntersections = maximum [countHLineTop,countHLineBottom,countVLineLeft,countVLineRight]
																element = elemIndex maxNoIntersections tempList
																(returnLine,returnBool,maybeIntersection)= case element of
																			  		Just 0 -> (hLineTop,boolHLineTop,Horizontal)
																					Just 1 -> (hLineBottom,boolHLineBottom,Horizontal)
																					Just 2 -> (vLineLeft,boolVLineLeft,Vertical)
																					Just 3 -> (vLineRight,boolVLineRight,Vertical)
																intersection = if maxNoIntersections >= 2 then maybeIntersection
																	   	else NoIntersection		
																midPt = case intersection of
																			Horizontal -> computeMidPt returnLine returnBool 
																			otherwise -> 0 
																computeMidPt pts bools =  findAngle
																								where
																									(minPt,minRef) = minIntersection pts bools 0
																									minIntersection (p:ps) (b:bs) minIndex =
																										if b==True then (p,minIndex)
																											else minIntersection ps bs (minIndex+1) 
																									minIntersection [] [] minIndex = (pts !! (minIndex-1),minIndex)
																									(maxPt,maxRef) = maxIntersection (drop minRef pts) (drop minRef bools) minRef 
																									maxIntersection (p:ps) (b:bs) maxIndex =  
																										if b==False then (p,maxIndex)
																											else maxIntersection ps bs (maxIndex+1)  
																									maxIntersection [] [] maxIndex = (pts !! (maxIndex-1),maxIndex)
																									findTupleMidPt (x1,y1) (x2,y2) = (x1+x2)/2 
																									midPt = findTupleMidPt 	minPt maxPt
																									findAngle = (midPt-paddleMid )/l
																noIntersections (x:xs) count = if x == True 
																									then noIntersections xs (count +1) 
																									else noIntersections xs count   									
																noIntersections [] count = count	   						
												insideCircle (x,y) = if (x -xc)^2 + (y-yc)^2 <= r^2 then True else False			
												step = 1 / 5 																								
												


intersectionRectRect :: (Double,Double) -> (Double,Double) -> (Double,Double) -> (Double,Double) -> Bool
intersectionRectRect (x1,y1) (length1,breadth1) (a,alpha) (length2,breadth2) = cond1 && cond2 && cond3  && cond4
																					where
																				 		x2 = x1 + length1
																				 		y2 = y1 + breadth1
																				 		b = a + length2
																				 		beta = alpha + breadth2
																						cond1 = a < x2
																				 		cond2 = b > x1 
																						cond3 = alpha  < y2
																						cond4 = beta > y1
																						

intersectionRectCircle :: (Double,Double) -> (Double,Double) -> (Double,Double,Double)  -> Bool
intersectionRectCircle  (x1,y1) (l,b)  (xc,yc,r) = intersectionRectRect (x1,y1) (l,b) (xc-r,yc-r) (2 * r,2 *r)
				

actionListenerBallPaddle::DeviceContext -> MVar (Key,BallKey)  -> IO ()
actionListenerBallPaddle context lock  =  do
		 						event <- wait context 
								(pKey,bKey)	<-takeMVar lock								
								let (pKeyPress,bKeyPress) = case (eWhich event) of  
													Just 87 -> (pKey,MouseUp)
													Just 65 -> (pKey,MouseLeft)
													Just 83 -> (pKey,MouseDown)
													Just 68 -> (pKey,MouseRight)
													Just 37 -> (LeftKey,bKey)
													Just 39 -> (RightKey,bKey)
													Just 80 -> (PauseKey,bKey)
													otherwise -> (NoKey,No) 
								--print ("Key Pressed " ++ (show pKeyPress))												
								--print ("Mouse Press " ++ (show bKeyPress))				
								putMVar lock (pKeyPress,bKeyPress) 
								actionListenerBallPaddle context lock 												


actionListenerPaddle::DeviceContext -> MVar Key  -> IO ()
actionListenerPaddle context lock  =  do
		 						event <- wait context 
								takeMVar lock
								let keyPressed = case (eWhich event) of  
												Just 37 -> LeftKey
												Just 39 -> RightKey
												Just 80 -> PauseKey
												otherwise -> NoKey 
								print ("Key Pressed " ++ (show keyPressed))				
								putMVar lock keyPressed 
								actionListenerPaddle context lock 												

actionListenerBall::DeviceContext -> MVar BallKey  -> IO ()
actionListenerBall context lock  =  do
		 						event <- wait context 
								takeMVar lock
								let keyPressed = case (eWhich event) of  
												Just 87 -> MouseUp
												Just 65 -> MouseLeft
												Just 83 -> MouseDown
												Just 68 -> MouseRight
												otherwise -> No 
								print ("Mouse Press " ++ (show keyPressed))				
								putMVar lock keyPressed 
								actionListenerBall context lock 												
								
drawPaddle:: (Double,Double) -> (Double,Double) -> Canvas ()
drawPaddle (x,y) (l,b) = do
							beginPath ()
							fillRect(x,y,l,b)
							fill()
							closePath ()
							stroke ()	

drawBall:: (Double,Double) -> Double -> Canvas ()
drawBall (x,y) r = do 
					let startAngle = 0 
					let endAngle = 2 * pi
					let clockWise = False
					beginPath ()
					arc (x, y, r,startAngle,endAngle,clockWise)
					fill ()
					closePath ()
putOnScreen:: [String] -> (Double,Double)-> Canvas() 
putOnScreen a (b,c) = fillText (Text.pack (toAString a),b,c)


toAString :: [String] -> String 
toAString (x:xs) = x ++ toAString xs
toAString [] = []								

								
toInt::(Double,Double) -> (Int,Int)
toInt (a,b)  = (fromIntegral (ceiling a), fromIntegral (ceiling b))
					
					
fmaptuple::(Double,Double) -> [(Double->Double)] -> (Double,Double)
fmaptuple (a,b) (f1:f2:[]) = (f1 a,f2 b)
fmaptuple (a,b) (f1:[]) = (f1 a,b)
fmaptuple (a,b) _ = (a,b)
						
multiplytuple::(Double,Double) -> [Double] -> (Double,Double)
multiplytuple (a,b) (f1:f2:[]) = (f1*a,f2*b)
multiplytuple (a,b) (f1:[]) = (f1*a,b)
multiplytuple (a,b) _ = (a,b)

calcRadius :: (Double,Double) -> [Double] -> Double
calcRadius (a,b) (w:h:[]) = (w*a) + (h*b)
calcRadius _ _ = 0





								