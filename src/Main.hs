module Main where

import Diagrams.Backend.Cairo.CmdLine
import Data.Active
import Diagrams.Prelude
import Mandelbrot
import AnimationExamples

main :: IO ()
main = mainWith $ withAnimation (1 :: Int) (stretch 10 $ loopify $ bg pink <$> circleHomotopy 0.0001)
