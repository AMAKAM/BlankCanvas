{-# LANGUAGE OverloadedStrings #-}
import Graphics.Blank
import Control.Monad
import Data.List
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.Text as Text
import qualified Graphics.Blank.Style as Style

type Dimension = (Double,Double)
type Percentage = Double
startBarStartPoint = (0.25,0.47)
startBarEndPoint = (0.75,0.53)

main:: IO ()
main = blankCanvas 4000 {events = ["keydown"]} $ \context -> do
							image <- send context ( do 	
									image <- newImage  "images/img.png"
									return image
									)
							startBarGradient <- send context (do
								i <- createLinearGradient (0,0,1,0)  
								i # addColorStop (0,"green") 
								i # addColorStop (0.5,"red")
								i # addColorStop (1,"blue")
								--addColorStopFromList [(0,"green"),(1,"white")] i
								return i
								) 
							let (startBarX,startBarY)  = multiplytuple startBarStartPoint [width context,height context]
							let (endBarX,endBarY) = multiplytuple startBarEndPoint [width context,height context]
							print ("(startBarX,startBarY)" ++ show ((startBarX,startBarY)))
							print ("(endBarX,endBarY)" ++ show ((endBarX,endBarY)))
							--print ("" ++ show ())
							
							send context (do
							beginPath ()
							rect (startBarX,startBarY,(endBarX - startBarX),(endBarY - startBarY))
							closePath ()
							lineWidth 3	
							stroke ()
								)	
							--print "Draw Outer Bar"									
							send context (do
								clearRect (startBarX,startBarY,endBarX,endBarY)
								
								)
							drawStartBar startBarGradient 10 (width context,height context) context		
							print "0"	
							threadDelay (20 * 20000)
							send context (do
								clearRect (startBarX,startBarY,endBarX,endBarY)
								
								)
							drawStartBar startBarGradient 20 (width context,height context) context		
							print "12"	
							threadDelay (20 * 20000)
							send context (do
								clearRect (startBarX,startBarY,endBarX,endBarY)
								
								)
							drawStartBar startBarGradient 30 (width context,height context) context		
							print "25"	
							threadDelay (20 * 20000)
							send context (do
								clearRect (startBarX,startBarY,endBarX,endBarY)
								
								
								)
							drawStartBar startBarGradient 50 (width context,height context) context		
							print "33"	
							threadDelay (20 * 20000)
							send context (do
								clearRect (startBarX,startBarY,endBarX,endBarY)
								
								
								)	
							drawStartBar startBarGradient 70  (width context,height context) context	
							print "55"	
							threadDelay (20 * 20000)
							send context (do
								clearRect (startBarX,startBarY,endBarX,endBarY)
								
								
								)
							drawStartBar startBarGradient 95 (width context,height context) context	
							print "77"		
							threadDelay (20 * 20000)
							send context (do
								clearRect (startBarX,startBarY,endBarX,endBarY)
								
								
								)	
							drawStartBar startBarGradient 100  (width context,height context) context
							threadDelay (20 * 20000)
							print "100"
							
drawStartBar::CanvasGradient -> Percentage  -> Dimension ->DeviceContext -> IO  () -- Canvas ()
drawStartBar gradient  percentage (w,h) context =	
		 					do
							let (startx,starty) = multiplytuple startBarStartPoint [w,h]
							let (endx,endy) = multiplytuple startBarEndPoint [w,h]								
							let (barStartX,barStartY) = (startx,starty)
							let barLength =  (endx - startx) /100 * percentage
							let barBreadth = (endy - starty) 
							print ("(startx,starty)" ++ show ((startx,starty)))
							print ("(endx,endy)" ++ show ((endx,endy)))
							print ("barLength" ++ show (barLength))
							print ("barBreadth" ++ show (barBreadth))
							send context (do
								beginPath ()
								Style.fillStyle gradient
								save()
								translate (barStartX,barStartY)
								scale (barLength,barBreadth)							
								fillRect (0,0,1,1)
								restore ()							
								closePath ()
								stroke ()								
								)
						
multiplytuple::(Double,Double) -> [Double] -> (Double,Double)
multiplytuple (a,b) (f1:f2:[]) = (f1*a,f2*b)
multiplytuple (a,b) (f1:[]) = (f1*a,b)
multiplytuple (a,b) _ = (a,b)