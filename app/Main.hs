module Main where

import System.Console.ANSI
import System.IO
import Control.Concurrent (threadDelay)
import Data.Complex
import Control.Monad (forM_)

-- Character patterns for different intensities
patterns :: [String]
patterns = 
    [ ".::::::::::::::::"
    , "*:::::::::::::::"
    , "**::::::::::::::"
    , "***:::::::::::::"
    , "****::::::::::::"
    , "*****:::::::::::"
    , "******::::::::::"
    ]

-- Mandelbrot iteration with smooth coloring
mandelbrot :: Complex Double -> Double
mandelbrot c = go (0 :+ 0) 0
  where
    go z iter
        | iter >= 100 = iter
        | mag > 2 = iter + 1 - log (log mag) / log 2
        | otherwise = go (z*z + c) (iter + 1)
        where mag = magnitude z

-- Convert point to colored character with animation effect
pointToColoredChar :: Double -> Int -> String
pointToColoredChar n time =
    let 
        baseColor = case (floor n) `mod` 6 of
            0 -> Blue
            1 -> Cyan
            2 -> Green
            3 -> Yellow
            4 -> Red
            _ -> Magenta
        
        intensity = if n > 50 then Vivid else Dull
        patternIndex = min 6 (floor (n / 15))
        char = patterns !! patternIndex !! (time `mod` 16)
        
    in setSGRCode [SetColor Foreground intensity baseColor] ++ [char] ++ setSGRCode [Reset]

-- Interesting points in the Mandelbrot set
interestingPoints :: [(Complex Double, String)]
interestingPoints =
    [ ((-0.7435669) :+ 0.1314023, "Spiral")
    , ((-0.16070135) :+ 1.0375665, "Tendril")
    , ((-0.7894673) :+ 0.1565492, "Mini")
    , (0.285 :+ 0.01, "Valley")
    ]

-- Generate frame
generateFrame :: Double -> Double -> Double -> Double -> (Int, Int) -> Int -> String
generateFrame x1 x2 y1 y2 (width, height) time =
    unlines 
        [ concat [ pointToColoredChar (mandelbrot (x :+ y)) time
                | x <- linearScale x1 x2 width ]
        | y <- linearScale y1 y2 height ]
    where
        linearScale start end n =
            [ start + (end - start) * (fromIntegral i) / (fromIntegral n - 1)
            | i <- [0..n-1] ]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    clearScreen
    hideCursor
    
    -- Multiple zooms into different interesting points
    forM_ interestingPoints $ \(point, name) -> do
        let frames = 50
        forM_ [0..frames] $ \i -> do
            let t = (fromIntegral i / fromIntegral frames) ** 0.5
                zoom = 2.0 * (1 - t) + 0.00001 * t
                
            setCursorPosition 0 0
            putStrLn $ "Exploring: " ++ name ++ " ***"  -- Windows-safe characters
            putStr $ generateFrame 
                (realPart point - zoom) (realPart point + zoom)
                (imagPart point - zoom) (imagPart point + zoom)
                (80, 40)  -- Larger size for better visuals
                i
                
            threadDelay 30000  -- Faster animation
            
        -- Pause briefly at the most zoomed point
        threadDelay 500000
    
    showCursor
    setSGR [Reset]