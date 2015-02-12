{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Blank                     	-- import the blank canvas

main :: IO ()
main = blankCanvas 3000 $ \ context -> do 	-- start blank canvas on port 3000
        send context $ do                 	-- send commands to this specific context
                moveTo(50,50)
                lineTo(200,100)
                lineWidth 10
                strokeStyle "red"
                stroke()                  	-- this draws the ink into the canvas

                me <- newCanvas (100,100)
                moveTo(150,150)
                lineTo(200,100)
                lineWidth 10
                strokeStyle "red"
                stroke()                  	-- this draws the ink into the canvas
				
                drawImage(me,[100,100])		