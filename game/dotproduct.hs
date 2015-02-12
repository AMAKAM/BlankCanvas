import System.IO
factor = 160
type Point = (Double,Double)

mainAngle = do 
			putStrLn "Pt X "
			ptX <- getLine
			putStrLn "Pt Y "
			ptY <-  getLine 
			putStrLn "Alpha "
			alpha <-  getLine 
			debugangle (read ptX,read ptY)  (read alpha)
			putStrLn "Next Iteration  "
			mainAngle
			
debugangle :: Point -> Double -> IO ()
debugangle  (ptX,ptY) alpha	=  do
								--putStrLn ("(nX,nY) " ++ (show (nx,ny)))
								--putStrLn ("(refX,refY) " ++ show ((refX,refY)))
								--putStrLn ("nx " ++ (show nx))
								--putStrLn (" " ++ (show nx))
								
								putStrLn ("dotProduct " ++ show (dotProduct))
								putStrLn ("Angle " ++ show (ang))
								putStrLn ("AngleRad " ++ show (angleRad))								
								putStrLn ("Deviation Factor " ++ show (deviationFactor))
								putStrLn ("returnAngle " ++ show (returnAngle))
								putStrLn ("returnAngle1 " ++ show (returnAngle1))
								putStrLn ("returnVector " ++ show (returnVector))
								--putStrLn (" " ++ show ())
								--putStrLn (" " ++ show ())
								where
									(nX,nY) = (0 - ptX,0 - ptY) 
									mag = sqrt (nX^2 + nY^2)
									(nx,ny) = (nX/mag,nY/mag) 
									(refX,refY) = if alpha > 0 then (-1,0) 	-- "inverted" Position Vector
													else (1,0)
									dotProduct = nx * refX + ny * refY  -- simplified as
									angleRad = acos (dotProduct)
									ang = 	toDeg angleRad
									deviationFactor = alpha * factor
									returnAngle = 90 - deviationFactor
									returnAngle1 = toRadian returnAngle 
									returnVector = (mag * (cos returnAngle1), mag * (sin returnAngle1))
									toRadian a = a * pi / 180
									toDeg a = a * 180/pi	

			
angle :: Point -> Double -> Double 
angle  (ptX,ptY) alpha	=  ang
							where
								(nX,nY) = (0 - ptX,0 - ptY) 
								mag = nX^2 + nY^2
								-- (nx,ny) = (nX/mag,nY/mag) simplified as 
								nx = nX/mag
								(refX,refY) = if alpha > 0 then (-1,0) 	-- "inverted" Position Vector
												else (1,0)
								--dotProduct = nx * refX + ny * refY simplified as
								dotProduct = nx * refX 
								angleRad = acos (dotProduct)
								ang = 	toDeg angleRad				
								toRadian a = a * pi / 180
								toDeg a = a * 180/pi	


main = 	do 
		putStrLn "Incident X "
		incidentVectorX <- getLine
		putStrLn "Incident Vector Y "
		incidentVectorY <- getLine 
	 	let (normalX,normalY) = (2,4)
		let (x,y) = (read incidentVectorX,read incidentVectorY)
		putStr "Angle  "
		putStrLn (show (calculateAngle_ (normalX,normalY) (x,y)))
		putStrLn "Next Iteration  "
		main
		
calculateAngle:: (Double,Double) -> (Double,Double) -> Double
calculateAngle (nx,ny) (iX,iY) = 	toDeg angle
								where
									(ix,iy) = (iX/(iX^2+iY^2),iY/(iX^2+iY^2))
									angle = acos $ (nx * ix) + (ny * iy)
									toRadian a = a * pi / 180
									toDeg a = a * 180/pi

calculateAngle_:: (Double,Double) -> (Double,Double) -> Double
calculateAngle_ (nX,nY) (iX,iY) = 	 angle
								where
									(ix,iy) = (iX/(iX^2+iY^2),iY/(iX^2+iY^2))
									(nx,ny) = (nX/(nX^2+nY^2),nY/(nX^2+nY^2))
									angle = acos $ (nX*iX + (nY*iY)) / ((iX^2+iY^2) * (nX^2+nY^2))
									toRadian a = a * pi / 180
									toDeg a = a * 180/pi									