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

rectAStartX = 0.809
rectAStartY = 0.01
rectALength = 0.09 
rectABreadth = 0.055
rectBLength = rectALength
rectBBreadth = rectABreadth
rectCStartX = rectAStartX 
rectCStartY = rectAStartY + rectABreadth 
rectCLength = rectALength
rectCBreadth = 2.1 * rectABreadth
rectDLength = rectCLength
rectDBreadth = rectCBreadth

rectAdimension = (rectAStartX,rectAStartY,rectALength,rectABreadth)
rectBdimension = (rectAStartX + rectALength ,rectAStartY ,rectBLength,rectBBreadth)
rectCdimension = (rectCStartX,rectCStartY,rectCLength,rectCBreadth)
rectDdimension = (rectCStartX + rectCLength ,rectCStartY,rectDLength,rectDBreadth)

main = blankCanvas 4000 {events = ["keydown"]} $ \context -> do
			send context (do
				rect ( mulitplyRect rectAdimension (width context,height context) )
				rect ( mulitplyRect rectBdimension (width context,height context) )
				rect ( mulitplyRect rectCdimension (width context,height context) )
				rect ( mulitplyRect rectDdimension (width context,height context) )								
				stroke ()	
				)
				
mulitplyRect :: (Double,Double,Double,Double) -> (Double,Double) -> (Double,Double,Double,Double)
mulitplyRect (x,y,l,b) (w,h)= (x*w,y*h,l*w,b*h) 				