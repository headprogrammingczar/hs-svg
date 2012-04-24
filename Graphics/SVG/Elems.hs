module Graphics.SVG.Elems where

import Graphics.SVG.Types

rect :: Float -> Float -> Float -> Float -> SVGTree
rect x y w h = Leaf emptyProps $ Rect (Point x y) w h

circle :: Float -> Float -> Float -> SVGTree
circle x y r = Leaf emptyProps $ Circle (Point x y) r

ellipse :: Float -> Float -> Float -> Float -> SVGTree
ellipse x y rx ry = Leaf emptyProps $ Ellipse (Point x y) rx ry

polyline :: [Point] -> SVGTree
polyline ps = Leaf emptyProps $ Polyline ps

line :: Point -> Point -> SVGTree
line a b = polyline [a,b]

path :: Path -> SVGTree
path p = Leaf emptyProps $ Path p

text :: Float -> Float -> String -> SVGTree
text x y s = Leaf emptyProps $ Text (Point x y) s

use :: String -> SVGTree
use s = Leaf emptyProps $ Use s

