import System.IO
factor = 10
main :: IO ()
main = do
		putStrLn "Enter dx"
		dx <- getLine 		
		putStrLn "Enter dy"
		dy <- getLine 		
		putStrLn "Enter value"
		value <- getLine 	
		putStr "(theta"
		putStr ",reflectionAngle" 
		putStr ",newDx"
		putStr ",newDy"
		putStr ",magnitude"
		putStr ",Pt"
		putStrLn ")" 
		putStrLn (show (calculateAngle (read dx) (read dy) (read value)))
		
			
calculateAngle:: Double -> Double -> Double -> (Double,Double)
calculateAngle bdx1 bdy1 angleValue = finalVector
							where 		
								xpositive = if bdx1 >= 0 then True else False
								ypositive = if bdy1 >= 0 then True else False
								xsqr1 = bdx1 ^ 2 
								ysqr1 = bdy1 ^ 2
								hsqr1 = xsqr1 + ysqr1
								magnitude = sqrt hsqr1
								hyp = magnitude
								theta =  toDeg (acos (abs (bdx1/hyp))) -- bdx is adjacent
								reflectionAngle =  90 - theta - (factor * (abs angleValue))
								newDx1 =  magnitude *(cos (toRadian reflectionAngle))
								newDy1 =  magnitude *(sin (toRadian reflectionAngle))
								newDx = if xpositive then newDx1 else (-1*newDx1)
								newDy = if ypositive then (-1*newDy1) else newDy1
								finalVector = (newDx,newDy)
								toRadian a = a * pi / 180
								toDeg a = a * 180/pi