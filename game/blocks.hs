{-# LANGUAGE OverloadedStrings #-}
import Graphics.Blank
import Control.Monad
import Data.List
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.Text as Text
import qualified Graphics.Blank.Style as Style



data Key = LeftKey | RightKey | PauseKey | NoKey
			deriving (Show)
data Intersection =   Horizontal | Vertical | Top | Bottom | BothWalls | NoIntersection  			
			deriving (Show,Eq)
data UserGameState = Running | GameOver | GameCompleted | LevelUp  
  			deriving (Show)
data PowerUp = DecreaseSize | IncreaseSize | MinSize | MaxSize | MultipleBalls | Life | Nuke | Gun | NoPowerUp 
			deriving (Show)
			
type Dimension = (Double,Double)
type PaddlePercentage =(Double,Double,Double)	 -- (edge,ring,main)	-- in decimal percentage  and sum of percentage 
																	-- 								not absolutely necessary though
type Rect = (Double,Double,Double,Double)				
type Color = Text.Text
type Point = (Double,Double)
type Vector = (Double,Double)
type XVector = Double
type Value = Double
type GameLevel = Int
type Level = Int
type Life = Int
type Line = (Point,Point)
 				

data Paddle = Paddle {
					 paddlePoint::Point
					,currentDimension::Dimension
					,xVector::XVector
					,paddleColor::Color
					,paddleGradient::CanvasGradient
				}						

data Ball = Ball {
					 ballPoint::Point
					,ballRadius::Double
					,ballVector::Vector
				}

data Bricks = Bricks	{
					isBrickDestroyable::Bool
					,getBrickDimension::Rect
					,getBrickLevel::Level
					,getBrickPower::PowerUp
					,getBrickGradient::Color	
				}

data GameState = GameState	{
					getImage::[CanvasImage]
					,gameLevel::GameLevel
					,gameDimension::Dimension
					,getPaddle::Paddle
					,getBall::Ball
					,getBricks::[Bricks]
					,getLife::Life	
					,getUserGameState::UserGameState			
				}
			
instance Show (Paddle) where
			show paddle = "Point" ++ (show (paddlePoint paddle))

--GameMathFormulaToHold
------------ Game
------------ multiple of 100 w 
------------ multiple of 50 h

------------ Paddle
------------ minSize >= 3 * StepSize
------------ defaultSize >= 5*StepSize
------------ maxSize >= 7 * StepSize

------------ Ball
------------ defaultBallPoint >> defaultBallRadius 
------------ defaultBallVector < minStepSize
------------ minSizePaddleLength > 3 minSizePaddleBreadth
------------ factor < (90 / abs(alphaMax))  


------------ minWidth 750
------------ maxHeight 700

------------ min Size = 3 * StepSize
------------ defaultSize = 5 
------------ maxSize = 7 * StepSize 
------------ StepSize = 0.025
------------ defaultBallPoint = (0.09,0.09)
------------ defaultBallRadius = (0.005,0.005)




--Paddle Defintions all Percentages
--StartPoint 				
defaultPaddlePoint=(0.00,0.93)
-- Step Size 
stepSize = 0.025 
--Paddle Dimensions	as a Factor
breadthPaddle = 0.02
defaultPaddleDimension = (5 * stepSize, breadthPaddle)
defaultMinDimension = (3 * stepSize,breadthPaddle)
defaultMaxDimension = (7 * stepSize,breadthPaddle)
--XVectorFactor
defaultPaddleSpeed =  1 * stepSize



--Ball Object 
defaultBallPoint = (0.03,0.03)
defaultballRadius = (0.005,0.005) -- as a percentage of height and width 
defaultBallVector = (0.008,0.011)
factor = 100

--Colors
defaultPaddleColor = "lightgreen"
pColor = defaultPaddleColor
defaultBallColor = "green" 

--Start Level
startLevel = 1;

{-
test = blankCanvas 3000 $ \ context -> 	do
										let w = if 1300  <= (width context) then 1300 
												else if 1200  <= (width context) then 1200
													else if 1100  <= (width context) then 1100
																						else 1
																
										let h = if 750  <= (height context) then 750 
												else if 650  <= (height context) then 600
													else if 550  <= (height context) then 550
																						else 1	
										loop context (100,100) (1.0,0.5) (w,h)
										
loop::DeviceContext -> Dimension -> Dimension -> Dimension -> IO ()
loop context (x,y) (dx,dy)	(w,h)= do 
									let x1 = dx+ x 
									let y1 = dy+ y
									let ballRad = (calcRadius defaultballRadius [w,h])
									print (x1,y1)
									send context (do
										clearRect (0,0,w,h)
										globalCompositeOperation "source-over"
										fillStyle defaultBallColor
										drawBall (x1,y1) ballRad
										)
									threadDelay (20 * 10000)	
									loop context (x1,y1) (dx,dy) (w,h)
-}								
main = blankCanvas 4000 {events = ["keydown"]} $ \context -> do
																
																let w = if 1300  <= (width context) then 1300 
																		  	else if 1200  <= (width context) then 1200
																			 		else if 1100  <= (width context) then 1100
																					  	else if 1000  <= (width context) then 1000
																						 		else if 900  <= (width context) then 900
																								  	else if 800  <= (width context) then 800
																									 		else if 700  <= (width context) then 700
																										 		else if 600  <= (width context) then 600
																												  	else if 500  <= (width context) then 500
																													 		else if 400  <= (width context) then 400
																															  	else if 300  <= (width context) then 300
																																 		else if 200  <= (width context) then 200
																																			else if 100  <= (width context) then 100
																																				else 0
																let h = if 750  <= (height context) then 750 
																		  	else if 650  <= (height context) then 650
																			 		else if 550  <= (height context) then 550
																					  	else if 450  <= (height context) then 450
																						 		else if 350  <= (height context) then 350
																								  	else if 250  <= (height context) then 250
																									 		else if 150  <= (height context) then 150
																											  	else 0
																												
																if w>0 && h >0
																	then (do																	
																		print ("(Width,Height) " ++ (show (w,h)))		
																		print ("BallRadius "++(show (calcRadius defaultballRadius [w,h])))
																		print ("BallVector "++ (show  (multiplytuple defaultBallVector [w,h])))
																		lock <- newMVar NoKey 
																		let dimension = (w,h)											
																		image<- send context ( do 	
																				image <- newImage  "images/img.png"
																				return image
																				)
				 														pGradient<-send context (do
																			i <- createLinearGradient (0,0,1,0)  
																			i # addColorStop (0,"green") 
																			i # addColorStop (0.5,"red")
																			i # addColorStop (1,"blue")
																			--addColorStopFromList [(0,"green"),(1,"white")] i
																			return i
																			)	
																		
																		forkIO (actionListener context lock)
																		renderLevel context lock GameState {
																						getImage=[image]
																						,gameLevel=startLevel
																						,gameDimension=dimension
																						,getPaddle= Paddle	{ 
																							 	paddlePoint=multiplytuple defaultPaddlePoint [w,h]
																								,currentDimension=defaultPaddleDimension	
																								,xVector=defaultPaddleSpeed 	
																								,paddleColor =defaultPaddleColor
																								,paddleGradient = pGradient
																							}
																						,getBall = Ball {
																								 ballPoint = multiplytuple defaultBallPoint [w,h]
																								,ballRadius =  calcRadius defaultballRadius [w,h]
																								,ballVector = multiplytuple defaultBallVector [w,h]
																							}
																						,getBricks = []
																						,getLife = 4
																						,getUserGameState = Running
																						}
																		)	
																	else 
																		(
																			do
																			notWorking context
																		)				

renderLevel:: DeviceContext -> MVar Key -> GameState  -> IO ()
renderLevel context lock gameState	 = do
						let level = gameLevel gameState 
						case level of
							1 -> (	
									do
									let paddle = getPaddle gameState
									let images = getImage gameState
									let level = gameLevel gameState
									let dimension = gameDimension gameState
									let ball = getBall gameState
									let life1 = getLife gameState
									let userGameState1 = getUserGameState gameState
									let (w,h) = dimension
									let (brickl1,brickb1) = multiplytuple (5*0.025,0.02) [w,h]
									let (x1,y1) = multiplytuple (0.2,0.2) [w,h]
									let (x2,y2) = multiplytuple (0.1,0.1) [w,h]
									let brick1 = Bricks 
														{
															isBrickDestroyable = True
															,getBrickDimension = (x1,y1,brickl1,brickb1) 
															,getBrickLevel = 1
															,getBrickPower = NoPowerUp
															,getBrickGradient = "green" 
														}
									let brick2 = Bricks 
														{
															isBrickDestroyable = True
															,getBrickDimension = (x2,y2,brickl1,brickb1)
															,getBrickLevel = 1
															,getBrickPower = NoPowerUp
															,getBrickGradient = "red"
														}					
									{-let brick1 = Brick 
														{
															isBrickDestroyable =  
															getBrickDimension = 
															getBrickLevel = 
															getBrickPower = 
															getBrickGradient =				
														} -}
									
									let newgameState = GameState {
																getImage=images
																,gameLevel=level
																,gameDimension=dimension
																,getPaddle= paddle
																,getBall = ball
																,getBricks = []
																,getLife = life1
																,getUserGameState = Running
															}
									gameLoop context lock newgameState							
								)
							otherwise -> (return ())

gameLoop::DeviceContext -> MVar Key -> GameState  -> IO ()
gameLoop context lock gameState	= do 
								-- get the gameState
									let paddle = getPaddle gameState
									let images = getImage gameState
									let level = gameLevel gameState
									let dimension = gameDimension gameState
									let ball = getBall gameState
									let bricks = getBricks gameState
									let life1 = getLife gameState
									let userGameState1 = getUserGameState gameState
									let (w,h) = dimension
									
									-- find key pressed
									key <- takeMVar lock
									putMVar lock NoKey
																		
									-- Loop until Pause State is resolved
									case key of 
										PauseKey -> (
													do
													let repeatLoop lock = (
																			do 
																			key <- takeMVar lock
																			putMVar lock NoKey
																			case key of  
																				PauseKey -> (	
																								return ()
																							)
																				otherwise -> (
																								do
																								threadDelay (20 * 2000)	
																								repeatLoop lock 
																							)	
																			)
													
													repeatLoop lock 	
													)
										otherwise -> (return ())
									
									case (userGameState1,context) of 
										(GameOver,_) ->             --- Game Over Loop in Here 
													(
													do
													send context 
														(   
															do
															clearRect (0,0,w+5,h+5)                     --
														)		
													) 				
										(otherwise,_) -> 
													(
													do 
													let returnPaddle = paddleAction paddle key (w,h)
													--let (returnBall,returnBricks,outOfBounds) = ballAction ball paddle bricks (w,h)	 
													(returnBall,returnBricks,outOfBounds) <- ballAction  context ball paddle bricks (w,h)	 
													
													
												-- GameState 
												-- depend on gameState do one of the following Actions 
												-- Leave it for now 
													
													send context (do
															clearRect (0,0,w+5,h+5)                     -- sometimes ball go out of bounds
															drawBackGroundImage images dimension level
															drawPaddle returnPaddle (w,h)
															drawBricks returnBricks 													
															drawBall returnBall
														)
														
												-- return OBJ --	
												-- final return objec	
													let (userGameState,life) = case outOfBounds of
																			True -> (GameOver,life1) 
																			False -> (Running,life1) 
													let returnGameState = GameState {
																	getPaddle=returnPaddle
																	,getImage=images
																	,gameLevel=level
																	,gameDimension=dimension
																	,getBall=returnBall
																	,getBricks = returnBricks
																	,getLife = life 
																	,getUserGameState = userGameState
															}																
													threadDelay (20 * 2000)
													gameLoop context lock returnGameState
													)	
									
									

paddleAction :: Paddle -> Key -> Dimension -> Paddle 											 
paddleAction  paddle key (w,h)= returnPaddle 
					where
						-- Paddle Objects									
							pdx = (xVector paddle) * w 
							pColor = paddleColor paddle
							(plengt,pbreadt) = multiplytuple (currentDimension paddle) [w,h]
							(px1,py) = paddlePoint paddle 
							gradient = paddleGradient paddle
							
						-- Find new Paddle position
							px = case key of 
									LeftKey -> if px1-pdx >= 0 then px1-pdx
																else px1
									RightKey -> if px1+ pdx + plengt <= w then px1 + pdx 
									 							else px1
									otherwise -> px1 										
						
						-- paddle Obj --
							_currentDimension = (currentDimension paddle)
							_xVector = (xVector paddle)
							_color = pColor									
							returnPaddle = Paddle	{ 	
															paddlePoint= (px,py)	
															,currentDimension=_currentDimension
															,xVector= _xVector	
															,paddleColor = pColor
															,paddleGradient = gradient
													}
											
						
ballAction :: DeviceContext -> Ball -> Paddle -> [Bricks] -> Dimension ->IO (Ball,[Bricks],Bool)  
ballAction context ball paddle bricks (w,h) =  
					(
					do					
					send context (do
							save ()
							scale (1,2)
							--putOnScreen ["(paddleIntersection,wallIntersection,outOfBounds,brickIntersection)"] (300,100)
						--putOnScreen [show (paddleIntersection,wallIntersection,outOfBounds,brickIntersection)] (300,300)							
							restore ()
							)
					return (returnBall,returnBricks,outOfBounds)		
					)
					where
					-- Paddle Object
						(plengt,pbreadt) = multiplytuple (currentDimension paddle) [w,h]
						(px,py) = paddlePoint paddle
					
					-- BallObject
						(bx1,by1) = ballPoint ball
						ballRad = ballRadius ball
						(bdx1,bdy1) = ballVector ball 	
					
					
						
						
					--Find Collision with Paddle,Wall,Bricks 
						(paddleIntersection,alpha) = findIntersectionAxisPaddle (px,py) (plengt,pbreadt) (bx1,by1,ballRad)	
						wallIntersection = findIntersectionWall (w,h)  (bx1,by1,ballRad)							
						(returnBricks,brickIntersection) = returnTuple (bdx1,bdy1)
							where
							(noIntersectionBricksList,intersectionTupleBricksList) =  findIntersectingNonIntersectingBricks bricks										
							returnTuple (bdx1,bdy1) = case length intersectionTupleBricksList of
								0 -> (noIntersectionBricksList,NoIntersection)
								1 ->  (returnBricks1,intersectionPattern1)
										where
											((brick1,intersectionPattern1):[]) = intersectionTupleBricksList  			
											brickDestroyable1 = isBrickDestroyable brick1 
										 	returnBricks1 = if brickDestroyable1 == True then brick1:noIntersectionBricksList
																							else noIntersectionBricksList
							
								2 -> 	case getPattern intersectionTupleBricksList of 
											(Horizontal:Horizontal:[])-> case bdx1>0  of  -- is ball coming down
																			True -> case lessThanEqualMinimumY of --ball coming down
																						(True,False) -> if brickDestroyable1 == True  then (brick2:noIntersectionBricksList,Horizontal)
																																		else (brick1:brick2:noIntersectionBricksList,Horizontal)
																																		
																						(False,True) ->if brickDestroyable2 == True  then (brick1:noIntersectionBricksList,Horizontal)
																																		else (brick1:brick2:noIntersectionBricksList,Horizontal)
																										
																						(True,True) -> case (brickDestroyable1,brickDestroyable2) of
																											(True,True) -> (noIntersectionBricksList,Horizontal)
																											(True,False)-> (brick2:noIntersectionBricksList,Horizontal)
																											(False,True)-> (brick1:noIntersectionBricksList,Horizontal)
																											(False,False)-> (noIntersectionBricksList,Horizontal)
																						
																			 
																			False -> case greaterThanEqualMaximumY of --ball going up
																						(True,False) -> if brickDestroyable1 == True  then (brick2:noIntersectionBricksList,Horizontal)
																																		else (brick1:brick2:noIntersectionBricksList,Horizontal)
																																		
																						(False,True) ->if brickDestroyable2 == True  then (brick1:noIntersectionBricksList,Horizontal)
																																		else (brick1:brick2:noIntersectionBricksList,Horizontal)
																										
																						(True,True) -> case (brickDestroyable1,brickDestroyable2) of
																											(True,True) -> (noIntersectionBricksList,Horizontal)
																											(True,False)-> (brick2:noIntersectionBricksList,Horizontal)
																											(False,True)-> (brick1:noIntersectionBricksList,Horizontal)
																											(False,False)-> (noIntersectionBricksList,Horizontal)
																			 
																		
										where 
											minimumY = min brick1MinY brick2MinY
											maximumY = max brick1MaxY brick2MaxY 
											lessThanEqualMinimumY = (brick1MinY==minimumY,brick2MinY==minimumY)
											greaterThanEqualMaximumY = (brick1MaxY==maximumY,brick2MaxY==maximumY)
											((brick1,_):(brick2,_):[]) = intersectionTupleBricksList  
											(brick1X,brick1Y,brick1L,brick1W) = getBrickDimension brick1 
											(brick2X,brick2Y,brick2L,brick2W) = getBrickDimension brick2
											brick1MinX = brick1X
											brick1MaxX =  brick1X + brick1L
											brick1MinY = brick1Y
											brick1MaxY = brick1Y + brick1W
											brick2MinX = brick2X
											brick2MaxX = brick2X + brick2L
											brick2MinY = brick2Y
											brick2MaxY = brick2Y + brick2W
											brickDestroyable1 = isBrickDestroyable brick1 
											brickDestroyable2 = isBrickDestroyable brick2 
																						
										--(Vertical:Vertical:[]) -> 	
										--otherwise only possibility (Horizontal:Vertical:[]) 
										--otherwise ->
										
										
									   
													
							-- Get Pattern of Intersection 
							getPattern ((_,intersection):tuples) =  intersection : getPattern tuples	
							getPattern  [] = []
													
							-- returns a two Tuples of List,Tuple = (nonIntersectingBricks,(intersectingBrick,intersectionAxis))
							findIntersectingNonIntersectingBricks bricks = splitBricksOnIntersection bricks ([],[])
							splitBricksOnIntersection (brick:bricks) (noIntersectingList,intersectingTuple)= 
								if brickIntersection == NoIntersection then splitBricksOnIntersection bricks (brick:noIntersectingList,intersectingTuple)
																			else splitBricksOnIntersection bricks (noIntersectingList,(brick,brickIntersection):intersectingTuple)
																		where
																			
																			--brickDestroyable = isBrickDestroyable brick 
																			brickDimension = getBrickDimension brick
																			brickIntersection = findIntersectionAxisBrick brickDimension (bx1,by1,ballRad)
																			--brickLevel =  getBrickLevel brick 
																			--brickPower = getBrickPower brick 
																			--brickGradient = getBrickGradient brick 
							splitBricksOnIntersection [] (noIntersectingList,intersectingTuple) = (noIntersectingList,intersectingTuple)											
								-------------------
								
							--findCollidingBrick tuple:tuples = if intersection /= NoIntersection  then (Maybe brick,tuples) 
							--																			else findCollidingBrick tuples 
							--									where
							--										let intersection = snd tuple										
							-- findCollidingBricks [] = (Nothing,[]) 						
														 
						
					--Find if wallIntersection is outOfBounds or not
						outOfBounds = case wallIntersection of
											Horizontal -> if by1 + ballRad > h then True 
																	else False 
											BothWalls -> if by1 + ballRad > h then True 
																	else False 						
											otherwise ->  False					
					-- Find Intersection and Calculate new Ball Vector 
						(bdx,bdy) = case (paddleIntersection,wallIntersection,brickIntersection) of
											(NoIntersection,wall,NoIntersection) -> case wall of 
																						NoIntersection  -> (bdx1,bdy1)
																						Vertical -> (-1 * bdx1,bdy1)
																						Horizontal -> (bdx1,-1 * bdy1)
																						BothWalls -> (-1 * bdx1,-1 * bdy1)																								
											(paddleInt,NoIntersection,NoIntersection) -> case paddleInt of
																	Top -> finalReturnVector 
																				where
																					--(nX,nY) = (0 - ptX,0 - ptY) 
																					--mag = sqrt (nX^2 + nY^2)
																					--(nx,ny) = (nX/mag,nY/mag) 
																					--(refX,refY) = if alpha > 0 then (-1,0) 	-- "inverted" Position 
																					--					else (1,0)
																					--dotProduct = nx * refX + ny * refY  -- simplified as
																					--angleRad = acos (dotProduct)
																					--ang = 	toDeg angleRad
																					magV = sqrt (bdx1^2 + bdy1^2)
																					deviationFactor = abs (alpha * factor)
																					returnAngle = 90 - deviationFactor
																					returnAngle1 = toRadian returnAngle
																					(returnX,returnY) = (magV * (cos returnAngle1), magV * (sin returnAngle1))
																					finalReturnVectorX = if alpha>=0 then returnX else (0 - returnX) 
																					finalReturnVectorY = 0 - (abs returnY)
																					finalReturnVector = (finalReturnVectorX,finalReturnVectorY)	   
																					toRadian a = a * pi / 180
																					toDeg a = a * 180/pi																												
																	otherwise -> (bdx1, -1 * bdy1)        
											(NoIntersection,wall,brickIntersection) -> case (wall,brickIntersection) of 
																								(NoIntersection,Horizontal) ->(bdx1,-1 * bdy1)
																								(NoIntersection,Vertical) ->(-1 * bdx1,bdy1)
																								(Vertical,Vertical) ->(-1 * bdx1,bdy1)
																								(Horizontal,Horizontal) ->(bdx1,-1 * bdy1)
																								(BothWalls,_) -> (-1 * bdx1,-1 * bdy1)
																								otherwise -> (-1 * bdx1,-1 * bdy1)
																									--(H,V)
																									--(V,H)
											
					-- Find new Ball Position 								
						bx = bx1 + bdx 
						by = by1 + bdy
						returnBall = Ball 	{
												 ballPoint = (bx,by)
												,ballRadius = ballRad	
												,ballVector = (bdx,bdy)	
											}
						
					

					
actionListener::DeviceContext -> MVar Key  -> IO ()
actionListener context lock  =  do
		 						event <- wait context 
								takeMVar lock
								let keyPressed = case (eWhich event) of  
												Just 37 -> LeftKey
												Just 39 -> RightKey
												Just 80 -> PauseKey
												otherwise -> NoKey 
								--print ("Key Pressed" ++ (show keyPressed))				
								putMVar lock keyPressed 
								actionListener context lock 

																	
findIntersectionAxisPaddle:: (Double,Double) -> (Double,Double) -> (Double,Double,Double)  -> (Intersection,Value)  
findIntersectionAxisPaddle (xp,yp) (l,b)  (xc,yc,r)  = intersects 
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
																			  		Just 0 -> (hLineTop,boolHLineTop,Top)
																					Just 1 -> (hLineBottom,boolHLineBottom,Bottom)
																					Just 2 -> (vLineLeft,boolVLineLeft,Vertical)
																					Just 3 -> (vLineRight,boolVLineRight,Vertical)
																intersection = if maxNoIntersections > 0 then maybeIntersection
																	   	else NoIntersection		
																midPt = case intersection of
																			Top -> computeMidPt returnLine returnBool 
																			otherwise -> 0 
																computeMidPt pts bools =  findAlpha
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
																									middlePt = findTupleMidPt 	minPt maxPt
																									findAlpha = (middlePt-paddleMid )/l
																noIntersections (x:xs) count = if x == True 
																									then noIntersections xs (count +1) 
																									else noIntersections xs count   									
																noIntersections [] count = count	   						
												insideCircle (x,y) = if (x -xc)^2 + (y-yc)^2 <= r^2 then True else False			
												step = 1 / 5 																								

findIntersectionAxisBrick:: Rect -> (Double,Double,Double)  -> Intersection  
findIntersectionAxisBrick (xp,yp,l,b)  (xc,yc,r)  = intersects 
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
																	NoIntersection
												calculateIntersection =  intersection
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
																intersection = if maxNoIntersections > 0 then maybeIntersection
																	   	else NoIntersection		
																noIntersections (x:xs) count = if x == True 
																									then noIntersections xs (count +1) 
																									else noIntersections xs count   									
																noIntersections [] count = count	   						
												insideCircle (x,y) = if (x -xc)^2 + (y-yc)^2 <= r^2 then True else False			
												step = 1 / 5 																								
												

findIntersectionWall :: (Double,Double) -> (Double,Double,Double) -> Intersection 
findIntersectionWall (w,h)  (xc,yc,r)  = intersects 
												where
												(lr,br) = (2 * r,2 *r)
												(xr,yr) = (xc -r , yc-r)
												xMax = xc + lr
												yMax = 	yc + br
												intersects = case (cond1,cond2) of
																(True,True) -> NoIntersection
																(True,False) -> Horizontal
																(False,True) -> Vertical 
																otherwise -> BothWalls
													where 
														cond1 = xr >0 && xMax <w  -- vertical walls
														cond2 = yr >0 && yMax <h -- horizontal wall
														
					

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
				
								
-- Draw the background image from a list of image
drawBackGroundImage :: [CanvasImage] -> Dimension -> GameLevel -> Canvas()
drawBackGroundImage listImages (w,h) level = 	do 
															let image = head (drop (level-1) listImages)
															beginPath ()
															drawImage (image,[0,0,w,h])
															closePath ()
															stroke()



drawPaddle::Paddle -> Dimension -> Canvas ()
drawPaddle paddle (w,h) =
		 					do
							let pColor = paddleColor paddle
							let (plengt,pbreadt) = multiplytuple (currentDimension paddle) [w,h]
							let (px,py) = paddlePoint paddle 
							let gradient = paddleGradient paddle
							beginPath ()
							Style.fillStyle gradient
							save()
							translate (px,py)
							scale (plengt,pbreadt)							
							fillRect (0,0,1,1)
							restore ()							
							closePath ()
							stroke ()	
							
							--beginPath ()
							--fillStyle pColor
							--fillRect (px,py,plengt,pbreadt)
							--closePath ()
							--stroke ()	

drawBricks:: [Bricks] -> Canvas ()
drawBricks (brick:bricks) = do					
							beginPath ()
							fillStyle (getBrickGradient brick)
							fillRect (getBrickDimension brick)
							stroke ()
							closePath ()
							drawBricks bricks								
drawBricks [] = return ()

drawBall:: Ball  -> Canvas ()
drawBall ball = do 
					let	(bx,by) = ballPoint ball
					let	ballRad = ballRadius ball
					let startAngle = 0 
					let endAngle = 2 * pi
					let clockWise = False
					beginPath ()
					fillStyle defaultBallColor
					arc (bx,by,ballRad,startAngle,endAngle,clockWise)
					fill ()
					closePath ()

					
inRangePaddle::(Int,Int,Int) -> (Double,Double) -> (Double,Double) -> Intersection
inRangePaddle  (cx,cy,r) (x,y) (l,b) = 	if (yrange >= ymin)  && (yrange <= ymax) 
		 									then if (xrange >= xmin) && (cx+r <= xmax)   
												then Horizontal 
												else NoIntersection
											else NoIntersection				
										where 	
											yrange = cy+r
	 										xrange = cx+r
											ymin = floor y  
											ymax = fromIntegral (ceiling (y + b))
											xmin = floor x  
											xmax = fromIntegral (ceiling (x + b))										 
										
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
				
notWorking ::DeviceContext -> IO ()
notWorking context = do 
		 				send context (do
								putOnScreen ["Increase Screen Dimensions to play"] (10,10)
								stroke()
								fill()
							)
						print "Increase Screen Dimensions to play"	
						
putOnScreen:: [String] -> (Double,Double)-> Canvas() 
putOnScreen a (b,c) = fillText (Text.pack (toAString a),b,c)


toAString :: [String] -> String 
toAString (x:xs) = x ++ toAString xs
toAString [] = []								
							
