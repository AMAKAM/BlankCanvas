{-# LANGUAGE OverloadedStrings #-}
import Graphics.Blank
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
--import qualified Graphics.Blank.Style as Gradient
import qualified Data.Text as Text
import qualified Graphics.Blank.Style as Style



							
data GameState = GameState	{
								batCoordinates :: (Double,Double)
								,otherValues::Int
							}
							
instance Show (GameState) where 
	show GameState{ batCoordinates=(a,b) ,otherValues=c }  = "Game State ( batCoordinates=(" ++ (show a) ++ "," ++ 
																		(show b) ++"), otherValue=" ++ (show c )++")"

main = blankCanvas 4000 { events = ["keydown"] } $ \context -> (preLoop context GameState { batCoordinates = (300,300) ,otherValues=75}) 

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
									globalCompositeOperation "source-over"
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
				stroke()
			
outerBlackBox ::DeviceContext -> Canvas ()
outerBlackBox context = do
						beginPath ()
  						rect(0,0,width context,height context)
						strokeStyle "black"
						lineWidth 5
						closePath()

					