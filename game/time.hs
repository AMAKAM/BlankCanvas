{-# LANGUAGE ScopedTypeVariables #-}
import System.Time
import Data.List.Split

data MONTH = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
				deriving (Show,Eq,Ord,Read)
main:: IO ()
main = do 
		clock <- getClockTime
		let (_:monthString:year:time:_) = splitWhen (==' ') (show clock) 
		let (hour:minute:seconds:_) = splitWhen (==':') (time)
		putStrLn (show clock)
		putStrLn hour
		putStrLn minute
		putStrLn seconds
		putStrLn (show (convertMonth monthString))
		putStrLn year 


convertMonth:: String -> MONTH
convertMonth	a	= read a
		
		
		
		