module AnimationExamples where

import Diagrams.Animation
import Data.Active

import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude
import Data.Ratio

type Anim b = Animation b V2 Double

example :: Anim B
example = bg pink <$> animEnvelope (translateX <$> (interval 0 5 |>> backwards (interval 0 5)) <*> circle 2)

smushFuncs :: (x -> b -> c -> d) -> (a -> b) -> (a -> c) -> x -> a -> d
smushFuncs f g h x a = f x (g a) (h a)

pathify :: (t -> (n, n)) -> t -> P2 n
pathify = (p2 .)

toparc :: (Floating t) => t -> (t, t)
toparc t = (-cos (pi * t), sin (pi * t))

botarc :: (Floating t) => t -> (t, t)
botarc t = (-cos (pi * t), -sin (pi * t))

constant :: (Floating t) => (t, t) -> t -> (t, t)
constant = const

circlePath :: (Floating t) => t -> (t, t)
circlePath t = (cos (2 * pi * t), sin (2 * pi * t))

convex :: (Floating n, Additive v) => n -> Point v n -> Point v n -> Point v n
convex t p q = (1 - t) *. p ^+^ t *. q

convexHomotopy :: (Floating n, Additive v) => (n -> Point v n) -> (n -> Point v n) -> (n -> n -> Point v n)
convexHomotopy = smushFuncs convex

parametric :: (Floating n, Enum n, TrailLike t) => n -> (n -> Point (V t) (N t)) -> t
parametric x f = fromVertices $ fmap f [0,x..1]

homotopyAnim :: (RealFloat n, Renderable (Path V2 n) B, Enum n) => n -> (n -> n -> Point V2 Double) -> Anim B
homotopyAnim clarity homotopy = fmap (lc red . strokeLine . parametric clarity) $ homotopy <$> interval 0 1

arcHomotopy :: Double -> Anim B
arcHomotopy clarity = animEnvelope (homotopyAnim clarity $ convexHomotopy (pathify toparc) (pathify botarc))

circleHomotopy :: Double -> Anim B
circleHomotopy clarity = animEnvelope (homotopyAnim clarity $ convexHomotopy (pathify circlePath) (pathify $ constant (1, 0)))

loopify :: Active a -> Active a
loopify anim = stretch (1 % 2) $ anim |>> backwards anim

withAnimation :: (Integral a) => a -> Anim B -> [(Diagram B, Int)]
withAnimation _ anim = zip frames frameDelays
  where
    frameDelays = repeat 1
    frames = simulate 10 anim
