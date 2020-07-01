module Mandelbrot where

import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

import Data.Complex

quadratic :: Complex Double -> Complex Double -> Complex Double
quadratic c z = z * z + c

orbit :: Complex Double -> Complex Double -> [Complex Double]
orbit c = iterate (quadratic c)

criticalOrbit :: Complex Double -> [Complex Double]
criticalOrbit = flip orbit 0

maxIter :: Int
maxIter = 64

magOut :: Double
magOut = 2.0

pixel :: [Complex Double] -> Int
pixel = length . takeWhile (\z -> magnitude z <= magOut) . take maxIter

edge :: Int
edge = 256

side :: Int -> Double -> Double -> [Double]
side n v0 v1 =
    let sv = (v1 - v0) / fromIntegral n
    in  [v0, (v0 + sv) .. v1]

sideX :: [Double]
sideX = side edge (-2) 2

sideY :: [Double]
sideY = side edge (-2) 2

outerProduct :: (a -> b -> c) -> [a] -> [b] -> [[c]]
outerProduct f as bs = map (\a -> map (f a) bs) as

grid :: [[Complex Double]]
grid = outerProduct (flip (:+)) sideY sideX

toSquare :: Int -> Diagram B
toSquare n = square 1 # lw medium # fc black # opacity (sqrt o)
    where
        o = fromIntegral n / fromIntegral maxIter

mandel :: [[Diagram B]]
mandel = map (map (toSquare . pixel . criticalOrbit)) grid

mandelImg :: Diagram B
mandelImg = vcat . map hcat $ mandel

mandelBG :: Diagram B
mandelBG = mandelImg # bgFrame 3 pink
