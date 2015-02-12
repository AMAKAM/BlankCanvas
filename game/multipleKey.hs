{-# LANGUAGE OverloadedStrings #-}
--import Criterion.Measurement
import Graphics.Blank
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
--import qualified Graphics.Blank.Style as Gradient
import qualified Data.Text as Text
import qualified Graphics.Blank.Style as Style
import           Data.Monoid((<>))
import           Data.List (nub)
import Data.List



data State = State
             { keys :: [Int]    -- key *codes* for pressed keys
             , step :: Int
             }
     deriving Show
	 
data Paddle = Paddle {
			dimension::(Double,Double,Double,Double)
		}

data Key = LeftKey | RightKey | PauseKey | NoKey | TopLeftKey | TopRightKey 
			deriving (Show)
	 
main1::IO ()
main1 = blankCanvas 3000 { events = ["keyup","keydown"] } $ \ context -> do
							lock <- newMVar (NoKey,NoKey,NoKey)  
							forkIO (findKeyPressed context lock [])
							let paddle1 = Paddle (10,10,40,40)
							let paddle2 = Paddle (10,90,40,40)
							loop1 context paddle1 paddle2 lock

loop1:: DeviceContext -> Paddle -> Paddle ->MVar(Key,Key,Key) ->IO ()
loop1 context paddle1 paddle2 lock = do 
								let (x1,y1,l1,b1) = dimension paddle1
								let (x2,y2,l2,b2) = dimension paddle2 
								(key1,key2,key3) <- takeMVar lock								
								let dx1 = case key1 of
										  LeftKey -> -80
										  RightKey -> 80
										  otherwise -> 0
								let dx2 = case key2 of 
											TopLeftKey -> -80
											TopRightKey -> 80
											otherwise -> 0
								when (dx1 /=0 && dx2/=0)   (print ("key1," ++ show (key1) ++ "key2," ++ show (key2)))																							 
								let returnPaddle1 = Paddle {dimension =(x1+dx1,y1,l1,b1)}
								let returnPaddle2 = Paddle {dimension = (x2+dx2,y2,l2,b2)}	

								putMVar lock (NoKey,NoKey,key3)
								send context (	do
									clearRect (0,0,width context,height context)
									drawBat (dimension returnPaddle1)
									drawBat (dimension returnPaddle2)
									)
								threadDelay (20 * 10000)
								loop1 context returnPaddle1 returnPaddle2 lock
								
						

findKeyPressed::DeviceContext -> MVar (Key,Key,Key) -> [Int] ->IO ()
findKeyPressed context lock keys = do 
		 							event <- wait context 
									(_,_,_)	<-takeMVar lock								
									let down_keys = case (eType event,eWhich event) of
							                          ("keydown",Just c) -> [c]
							                          _ -> []
									let up_keys = case (eType event,eWhich event) of
							                          ("keyup",Just c) -> [c]
							                          _ -> []
									let current_keys = [ k | k <- nub (keys ++ down_keys), not (k `elem` up_keys) ]
									print ("curent Keys" ++ show (current_keys))				  									
									let key1 = if elem 115  current_keys && (elem 100  current_keys /= True) then LeftKey
											else if elem 100  current_keys && (elem 115  current_keys /= True) then RightKey
												else NoKey											
									let key2 = if elem 107 current_keys && (elem 108  current_keys /= True) then TopLeftKey
											else if elem 108  current_keys && (elem 107  current_keys /= True) then TopRightKey
												else NoKey
									let key3 = if elem 80  current_keys then PauseKey else NoKey
									putMVar lock (key1,key2,key3)
									print ("(key1,key2,key3)"++ show ((key1,key2,key3)) )
									findKeyPressed context lock []
								
								
drawBat::(Double,Double,Double,Double)-> Canvas ()
drawBat (x,y,l,b) = do 
						beginPath ()
						rect (x,y,l,b)
						closePath ()
						stroke ()
						fill ()





main :: IO ()
main = blankCanvas 3000 { events = ["keyup","keydown"] } $ \ context -> loop context (State [] 0)

loop :: DeviceContext -> State -> IO a
loop context state = do
--        threadDelay (1 * 1000 * 10)    -- remove if writing a game
        send context $ do
                let (w,h) = (width context, height context)
                clearRect (0,0,w,h)
                lineWidth 1
                fillStyle "red"
                font "30pt Calibri"
                fillText("Keys currently pressed: " <> Text.pack (show (keys state)),50,50)
                fillText("Counter: " <> Text.pack (show (step state)),50,150)

--        print state

        control context state

control :: DeviceContext -> State -> IO a
control context state = do
        event <- wait context
        let down_keys = case (eType event,eWhich event) of
                          ("keydown",Just c) -> [c]
                          _ -> []
        let up_keys = case (eType event,eWhich event) of
                          ("keyup",Just c) -> [c]
                          _ -> []
        let current_keys = [ k | k <- nub (keys state ++ down_keys), not (k `elem` up_keys) ]
        let state' = state { step = step state + 1, keys = current_keys }
        loop context state'
