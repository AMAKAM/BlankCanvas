{-# LANGUAGE OverloadedStrings #-}
--import Criterion.Measurement
import Graphics.Blank
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
--import qualified Graphics.Blank.Style as Gradient
import qualified Data.Text as Text
import qualified Graphics.Blank.Style as Style

--import Data.Text.Internal
--Understand Currying
--function1:: Int -> (Int->Int) ->  Int  
--function1 a func = (func a) 
--xyz::Int -> Int -> Int 
--xyz a b = a + b  

--Left 37
--Right 39
--Pause p 80
--NoKey 

data Key = LeftKey | RightKey | PauseKey | NoKey
			deriving (Show)
type Dimension = (Double,Double)
type PaddlePercentage =(Double,Double,Double)	 -- (edge,ring,main)	-- in decimal percentage  and sum of percentage = 1 
																		-- 								not absolutely necessary though
type Color = Text.Text
type Point = (Double,Double)
type Vector = (Double,Double)
type XVector = Double
type Line = (Point,Point)
type GameLevel = Int
data Style = Style {
					strokeColor::Color
					,colorStop::[(Double,Color)]
				}
				deriving Show


data Paddle = Paddle {
					point::Point,
					defaultDimension::Dimension,
					currentDimension::Dimension,
					maxDimension::Dimension,
					minDimension::Dimension,
					paddlePercentage::PaddlePercentage,			
					xVector::XVector,
					edgeStyle::Style,
					ringStyle::Style,
					mainBatStyle::Style
				}						
data GameLogic = GameLogic {
					gameLevel::GameLevel
					,gameDimension::Dimension
				}

data GameState = GameState	{
					getPaddle::Paddle
					,getImage::[CanvasImage]
					,getGameLogic::GameLogic
					,reference::Int
				}
			
instance Show (Paddle) where
			show paddle = "Point" ++ (show (point paddle))
 				
defaultpointPaddle=(300,200)				
defaultPaddleDimension = (120,14)
defualtPaddlePercentage = (5,12.5,65) 
defaultPaddleSpeed = 30
defaultEdgePaddleStyle = 	Style { strokeColor="black"    	,colorStop = [(0,"black"),(1,"white")] 	}
defaultRingPaddleStyle = 	Style { strokeColor="black" 	,colorStop = [(0,"black"),(1,"brown")] 	}
defaultMainBatPaddleStyle = Style { strokeColor="black" 	,colorStop = [(0,"Green"),(1,"Blue")]	}
defaultPaddle = Paddle	{ 	point=defaultpointPaddle	,defaultDimension=defaultPaddleDimension 	,currentDimension=defaultPaddleDimension	,maxDimension=defaultPaddleDimension 	
							,minDimension=defaultPaddleDimension	,paddlePercentage=defualtPaddlePercentage 	,xVector=defaultPaddleSpeed 	,edgeStyle=defaultEdgePaddleStyle 	
							,ringStyle=defaultRingPaddleStyle,	mainBatStyle=defaultMainBatPaddleStyle
						}
startLevel = 1;
	


go context defaultPaddle= do
			 let h = height context
			 let w = width context 
			 let (x1,y1) = point defaultPaddle
			 p1<-send context ( do 	
			 	clearRect(0,0,w,h) 
			 	p<-drawPaddle defaultPaddle LeftKey (w,h)
			 	stroke ()
			 	return p
			  	)
			 threadDelay 10000
			 go context   Paddle	{ 			point =	(x1-10,y1)
												,defaultDimension=defaultPaddleDimension 	
												,currentDimension=defaultPaddleDimension	
												,maxDimension=defaultPaddleDimension 	
												,minDimension=defaultPaddleDimension	
												,paddlePercentage=defualtPaddlePercentage 	
												,xVector=defaultPaddleSpeed 	
												,edgeStyle=defaultEdgePaddleStyle 	
												,ringStyle=defaultRingPaddleStyle	
												,mainBatStyle=defaultMainBatPaddleStyle
											}
	






istest::IO ()
istest = blankCanvas 3000 {events= ["keydown"]} $ \context -> do
		 														i<-send context (do
																	i <- createLinearGradient (0,0,1,0)  
																	i # addColorStop (0,"green") 
																	i # addColorStop (0.5,"red")
																	i # addColorStop (1,"blue")
																	--addColorStopFromList [(0,"green"),(1,"white")] i
																	return i
																	)	
																--threadDelay (20 * 100000)				
																print "1A"	
																send context (do
																	clearRect(0,0,width context,height context)
																	Style.fillStyle i
																	save ()
																	translate (20,20)
																	scale (3,20)
																	--rect(20,20,33,20)
																	fillRect (0,0,1,1)
																	restore()
																	)
																	
																--threadDelay (20 * 10000)				
																print "2A"
																send context (do
																	clearRect(0,0,width context,height context)
																	Style.fillStyle i
																	save()
																	translate (200,20)
																	scale (13,20)
																	--rect(200,20,13,20)
																	fillRect (0,0,1,1)
																	restore ()
																	)
																print "3A"
																send context (do
																	clearRect(0,0,width context,height context)
																	Style.fillStyle i
																	save()
																	translate (180,30)
																	scale (13,20)
																	--rect(200,20,13,20)
																	fillRect (0,0,1,1)
																	restore ()
																	)
																print "4A"
																send context (do
																	clearRect(0,0,width context,height context)
																	Style.fillStyle i
																	save()
																	translate (220,50)
																	scale (13,20)
																	--rect(200,20,13,20)
																	fillRect (0,0,1,1)
																	restore ()
																	)
																print "5A"
																send context (do
																	clearRect(0,0,width context,height context)
																	Style.fillStyle i
																	save()
																	translate (200,20)
																	scale (13,20)
																	--rect(200,20,13,20)
																	fillRect (0,0,1,1)
																	restore ()
																	)			
															{-	threadDelay (20 * 10000)	
																print "3A"		
																send context (do
																	clearRect(0,0,width context,height context)
																	save()
																	translate (20,200)
																	scale (13,20)
																	Style.fillStyle i
																	--rect(20,200,13,20)
																	rect (0,0,1,1)
																	fill ()
																	stroke ()
																	restore ()
																	)	
																print "4A"	
																-}

addColorStopFromList :: [(Double,Color)] -> CanvasGradient -> Canvas ()
addColorStopFromList x i=   case x of 
							(stop,color):xs -> (
												do
													i # addColorStop (stop,color) 
													addColorStopFromList xs i
												)
							[] -> do
									Style.fillStyle i
									
									
												
fillGradientRect:: (Double,Double) -> (Double,Double) -> [((Double,Color))] -> Canvas() 
fillGradientRect (x1,y1) (lengt,breadt) (x) = do								
												beginPath ()		
												rect(x1,y1,lengt,breadt)
												i <- createLinearGradient (x1,y1,x1+lengt,y1) 
												addColorStopFromList x i							
												closePath ()
												--i # addColorStop (0,"green") 
												--i # addColorStop (0.5,"white")
												--i # addColorStop (1.0,"green") 				
												--Gradient.fillStyle i 
												fill ()
												stroke ()
													
									
drawBackGroundImage :: [CanvasImage] -> Dimension -> GameLevel -> Canvas()
drawBackGroundImage listImages (width,height) level = 	do 
															let image = head (drop (level-1) listImages)
															drawImage (image,[0,0,width,height])
															stroke()
										

																											
drawPaddle::Paddle -> Key -> Dimension -> Canvas Paddle 
drawPaddle paddle key (w,_)	= do 			
							let (x1,y) = point paddle
							let	dx = xVector paddle
							let x = case key of 
											LeftKey -> if x1-dx > 0 then x1-dx
																		else x1
											RightKey -> if x1+dx < w then x1 + dx 
										 								else x1
											otherwise -> x1 
							let	(defx,defy) = defaultDimension paddle

							let	(lengt,breadt) = currentDimension paddle
							let	(edgeP ,ringP,batP) = paddlePercentage paddle
							let (edge ,ring ,bat)	= (edgeP/100 ,ringP/100 ,batP/100)											 
							let (edgeX1,edgeY1)	= (x1,y)
							let (edgeLength,edgeBreadth) = (edge * lengt,breadt)
							let (ringX1,ringY1)	= (edgeX1 + edgeLength,y)
							let (ringLength,ringBreadth) = (ring * lengt,breadt)
							let (batX,batY) 	= (ringX1+ringLength ,y)
							let (batLength,batBreadth) = (bat * lengt,breadt ) 
							let (ringX2,ringY2) = (batX + batLength,y)
							let (edgeX2,edgeY2) = (ringX2 + ringLength,y)
							--putOnScreen (fmap show [(edge ,ring ,bat),(lengt,breadt,5)]) (10,20) 
							--putOnScreen (fmap show [(edgeX1,edgeY1),(edgeLength,edgeBreadth)]) (50,50) 
							--strokeStyle "black"
							--fillRect (x,y,lengt,breadt)		
							--putOnScreen (fmap (show) [(edgeX1,edgeY1),(edgeLength,edgeBreadth)]) (300,300)
							--putOnScreen (fmap (show) [edgeX1,edgeY1]) (300,300)
							
							fillGradientRect (edgeX1,edgeY1) (edgeLength,edgeBreadth) (colorStop (edgeStyle paddle)) 
							fillGradientRect (ringX1,ringY1) (ringLength,ringBreadth) (colorStop (ringStyle paddle)) 
							fillGradientRect (batX,batY) (batLength,batBreadth) (colorStop (mainBatStyle paddle)) 
							fillGradientRect (ringX2,ringY2) (ringLength,ringBreadth) (colorStop (ringStyle paddle)) 
							fillGradientRect (edgeX2,edgeY2) (edgeLength,edgeBreadth) (colorStop (edgeStyle paddle)) 
							
							return  Paddle	{ 	point=(x,y)	
												,defaultDimension=defaultPaddleDimension 	
												,currentDimension=defaultPaddleDimension	
												,maxDimension=defaultPaddleDimension 	
												,minDimension=defaultPaddleDimension	
												,paddlePercentage=defualtPaddlePercentage 	
												,xVector=defaultPaddleSpeed 	
												,edgeStyle=defaultEdgePaddleStyle 	
												,ringStyle=defaultRingPaddleStyle	
												,mainBatStyle=defaultMainBatPaddleStyle
											}
											
							--fillGradientRect (10,5) (70,30) (colorStop	(edgeStyle paddle))
							--fillGradientRect (100,100) (6,14) (colorStop	(edgeStyle paddle))
						
							--fillGradientRect (edgeX1,edgeY1) (edgeLength,edgeBreadth)  (colorStop	(edgeStyle paddle))

														
putOnScreen:: [String] -> (Double,Double)-> Canvas() 
putOnScreen a (b,c) = fillText (Text.pack (toAString a),b,c)


toAString :: [String] -> String 
toAString (x:xs) = x ++ toAString xs
toAString [] = []	




{-maintrial = blankCanvas 3000 {events= ["keydown"]} $ \context -> do 
																	let key =NoKey
																	let dimension = (width context,height context)
																	image<- send context ( do 	
																			image <- newImage  "images/img.png"
																			return image
																			)
																	send context (do		
																			drawBackGroundImage [image] dimension 1
																			globalCompositeOperation "source-over"
																			drawPaddle defaultPaddle key dimension
																			return ()
																			)	
																	
-}	
{-	
main = blankCanvas 4000 {events= ["keydown"]} $ \context -> do 
																print "Started Game"
																lock <- newMVar NoKey 
																--forkIO (actionListener context lock 1)
																image<- send context ( do 	
																		image <- newImage  "images/img.png"
																		return image
																		) 
																initializeLevel1 context lock (GameState {
																									getPaddle=defaultPaddle 
																									,getImage=[image] 
																									,getGameLogic=GameLogic {
																															gameLevel=startLevel	
																															,gameDimension = (width context,height context)
																														}	
																									,reference=1																													
																									}
																						) 
-}
																		
{-
initializeLevel1::DeviceContext -> MVar Key -> GameState ->IO ()																			
initializeLevel1 context lock gameState = 	do
											let images = getImage gameState		
											send context (do
												let gameLogic = getGameLogic gameState 
												let dimension = gameDimension gameLogic
												let gameLvl = gameLevel gameLogic 
												let paddle = getPaddle gameState
												let key = NoKey
												drawBackGroundImage images dimension gameLvl
												globalCompositeOperation "source-over"
												drawPaddle paddle key (gameDimension gameLogic)
												return ()
												)																
												
-}
{-																		
initializeLevel::DeviceContext -> MVar Key -> GameState ->IO ()
initializeLevel context lock gameState = 	
									do 	
									key <- takeMVar lock
									putMVar lock key
									print ("Before" ++ (show (getPaddle gameState)))
									let ref = reference (gameState)
									let images = getImage gameState	
									newPaddle <- send context (do
											let paddle = getPaddle gameState
											let gameLogic = getGameLogic gameState 
											let dimension = gameDimension gameLogic
											let gameLvl = gameLevel gameLogic 
											let key = key 
											globalCompositeOperation "source-over"
											drawBackGroundImage images dimension gameLvl
											newPaddle <- drawPaddle paddle key (gameDimension gameLogic)
											stroke()
											return newPaddle
											)
									print ("Ref" ++ show ref)
									print ("After" ++ (show newPaddle))			
									threadDelay (20 * 10000)		
									initializeLevel context lock 		 (GameState {
																						getPaddle= newPaddle
																						,getImage=images 
																						,getGameLogic=GameLogic {
																										gameLevel=startLevel	
																										,gameDimension = (width context,height context)
																												}		
																						,reference = ref + 1																													
																						}
																			) 

-}
{-									
actionListener::DeviceContext -> MVar Key -> Int -> IO ()
actionListener context lock int =  do
								print ("Enter Event Handler" ++ (show int))
		 						event <- wait context 
								takeMVar lock
								let keyPressed = case (eWhich event) of  
												Just 37 -> LeftKey
												Just 39 -> RightKey
												Just 80 -> PauseKey
												Nothing -> NoKey 
								putMVar lock keyPressed 
								actionListener context lock  (int +1)
								--actionListener context lock    			
-}							
						

																	

																
{- 
main = blankCanvas 3100 {events= ["keydown"]} $ \context -> do
																let x = 0 
																let h = height context
																let w = width context
																send context (do 
																	--beginPath ()
																	--rect(10,5,70,30)
																	--i <- createLinearGradient (10,5,80,5) 
																	--addColorStopFromList (colorStop defaultEdgePaddleStyle) i
																	--closePath ()
																	--fill ()
																	--stroke ()
																	clearRect (0,0,w,h)
																	fillGradientRect (10,5) (70,30) (colorStop defaultEdgePaddleStyle)
																	fillGradientRect (130,140) (30,50) (colorStop defaultEdgePaddleStyle)
																	)
																threadDelay(100*100)
																return ()
																			--beginPath ()
																			--rect(81,80,70,30)
																			--j <- createLinearGradient (81,80,151,80) 
																			--addColorStopFromList (colorStop defaultEdgePaddleStyle) j
																			--closePath()
																			--fill ()
																			--stroke ()
																			--fillGradientRect (10,5 ,70,30) 
																			
																			
-}



	{-
main = blankCanvas 3000 {events= ["keydown"]} $ \context -> do
																		(p1,image)<-send context ( do 	
																			clearCanvas 
																			image <- newImage  "images/img.png"
																			drawBackGroundImage [image] (100,100) 1
																			p<-drawPaddle defaultPaddle LeftKey (width context,height context)
																			return (p,image)
																			)
																		print p1
																		threadDelay (20 * 40000)		
																		
																		p2<-send context ( do 	
																			clearCanvas 
																			drawBackGroundImage [image] (100,100) 1
																			p<-drawPaddle p1 LeftKey (width context,height context)
																			return p
																			)
																		print p2
																		threadDelay (20 * 40000)		
																		
																		p3<-send context ( do
																			clearCanvas 	
																			drawBackGroundImage [image] (100,100) 1
																			p<-drawPaddle p2 LeftKey (width context,height context)
																			return p
																			)
																		print p3
																		threadDelay (20 * 40000)		
																		
																		
 
  -}
 
 {-
main = blankCanvas 3000 {events= ["keydown"]} $ \context -> do
																send context (do 
																			--beginPath ()
																			--rect(10,5,70,30)
																			--i <- createLinearGradient (10,5,80,5) 
																			--addColorStopFromList (colorStop defaultEdgePaddleStyle) i
																			--closePath ()
																			--fill ()
																			--stroke ()
																			fillGradientRect (10,5) (70,30) (colorStop defaultEdgePaddleStyle)
																			
																			--beginPath ()
																			--rect(81,80,70,30)
																			--j <- createLinearGradient (81,80,151,80) 
																			--addColorStopFromList (colorStop defaultEdgePaddleStyle) j
																			--closePath()
																			--fill ()
																			--stroke ()
																			fillGradientRect (10,5,70,30) 
																			
																			)

-}													
{- 
paddleStyle1=Style {} 
paddleStyle2=
paddleStyle3=					
defaultPaddle =Paddle { 
						point=(300,600) ,defaultdimension = (120,14) ,maxDimension =(120,14) ,dx=3 ,mainPaddleStyle=   
						currentDimension
					}

data Ball = Ball {
					centre::Point,
					radius::Double,
					movementVector::Vector,
					maxVector::Vector,
					minVector::Vector
				}

data Brick = Brick	{
					hLine::[Line],
					vLine::[Line]
				}
				
data Wall = Wall {
					hWallLine::[Line],
					vWallLine::[Line],
					getWallStyle::Style
				}
				
newtype Image = Image {getimage::CanvasImage}

data GameState = GameState {
								paddle::Paddle
							}
defaultPaddle::Paddle

	
initialState::GameState 
initialState = GameState {paddle=defaultPaddle}
-}



{-	
main = blankCanvas 3000 {events= ["keydown"] } $ \context -> do
															send context $ do
																beginPath ()
																rect (10,10,25,10)
																i <- createLinearGradient (10,10,25,10)  
																i # addColorStop (0,"green") 
																i # addColorStop (0.5,"white")
																i # addColorStop (1.0,"green") 				
																Gradient.fillStyle i
																fill ()
																stroke ()
-}
																

--initializeGame

{-
							
data GameState = GameState	{
								batCoordinates :: (Double,Double)
								,otherValues::Int
							}
							
instance Show (GameState) where 
	show GameState{ batCoordinates=(a,b) ,otherValues=c }  = "Game State ( batCoordinates=(" ++ (show a) ++ "," ++ 
																		(show b) ++"), otherValue=" ++ (show c )++")"
-} 
--main2 = blankCanvas 3000 $ \context -> trialAnimation context
--trialAnimation::DeviceContext -> GameState -> IO ()
{-
main = blankCanvas 4000 { events = ["keydown"] } $ \context -> (preLoop context GameState { batCoordinates = (300,600) ,otherValues=75}) 

preLoop::DeviceContext -> GameState -> IO ()
preLoop context gameState = do
								image<- send context ( do 	
											image <- newImage  "images/img.png"
											return image
											)
								loop context gameState image 
								threadDelay (20 * 700)
	

loop::DeviceContext -> GameState -> CanvasImage -> IO ()
loop context gameState image = 	do 
								send context (do
									clearRect(0,0,width context,height context) 
									canvasOperations context gameState image
										) 
								eventAction context gameState image

eventAction::DeviceContext -> GameState -> CanvasImage-> IO ()
eventAction context gameState image  = do 
										event <- wait context
										print (eWhich event)
										when ((otherValues gameState) > 0) (loop context (changeGameState gameState moveBatLeft) image)
									

				--(loop context (changeGameState gameState moveBatLeft))
				

canvasOperations::DeviceContext -> GameState -> CanvasImage-> Canvas () 
canvasOperations context gameState img = do				
				globalCompositeOperation "source-over"
				drawImage(img,[0,40,790,645,2,2,width context,height context])				
				outerBlackBox context
				drawBat (batCoordinates gameState) 
				stroke ()
				--putOnScreen [show (otherValues gameState)] (100,100)


				
moveBatLeft::((Double,Double),Int) -> ((Double,Double),Int)		
moveBatLeft = \((a,b),c) -> ((a-1,b),c-1) 		
				
changeGameState:: GameState -> (((Double,Double),Int)->((Double,Double),Int))-> GameState  
changeGameState GameState{ batCoordinates=(a,b) ,otherValues=c } fn = let ((d,e),f) = fn ((a,b),c)
	  																in GameState (d,e) f 
	
	
putOnScreen::[String] -> (Double,Double)-> Canvas() 
putOnScreen a (b,c) = fillText (Text.pack (toAString a),b,c)



toAString :: [String] -> String 
toAString (x:xs) = x ++ toAString xs
toAString [] = []
				
drawBat ::(Double,Double) -> Canvas()
drawBat (x,y) = do
				beginPath ()
		  		strokeStyle "Blue"
		  		lineWidth 2
		  		fillStyle "green"
		  		rect (x,y,120,14)
				closePath ()
		  		fill ()
				closePath ()
			
outerBlackBox ::DeviceContext -> Canvas ()
outerBlackBox context = do
						beginPath ()
  						rect(0,0,width context,height context)
						strokeStyle "black"
						lineWidth 5
						closePath()
-}
					