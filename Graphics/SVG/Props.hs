module Graphics.SVG.Props where

import Graphics.SVG.Types

fill :: String -> SVGTree -> SVGTree
fill c = addAttribute (Attribute "fill" c)

stroke :: String -> SVGTree -> SVGTree
stroke c = addAttribute (Attribute "stroke" c)

addAttribute :: Attribute -> SVGTree -> SVGTree
addAttribute a (Leaf p e) = Leaf (appendAttribute p a) e
addAttribute a (Node p b ts) = Node (appendAttribute p a) b ts

appendAttribute :: Props -> Attribute -> Props
appendAttribute (Props as) a = Props (a:as)

