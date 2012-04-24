module Graphics.SVG.Blocks where

import Graphics.SVG.Types

svg :: [SVGTree] -> SVGTree
svg = Node emptyProps Root

g :: [SVGTree] -> SVGTree
g = Node emptyProps Group

defs :: [SVGTree] -> SVGTree
defs = Node emptyProps Defs

symbol :: [SVGTree] -> SVGTree
symbol = Node emptyProps Symbol

