module Graphics.SVG.Types where

import Data.Monoid

data SVGTree = Leaf Props Elem | Node Props Block [SVGTree] deriving (Show)

-- Attribute "id" "awesomeElement"
-- styles are implemented as presentation attributes, for simplicity
data Attribute = Attribute String String deriving (Show)

data Props = Props [Attribute] deriving (Show)

emptyProps :: Props
emptyProps = mempty

appendProps :: Props -> Props -> Props
appendProps = mappend

instance Monoid Props where
  mempty = Props []
  (Props a) `mappend` (Props c) = Props (a++c)

data Elem = Rect Point Float Float
          | Circle Point Float
          | Ellipse Point Float Float
          | Polyline [Point]
          | Path [PathData]
          | Text Point String
          | Use String
          deriving (Show)

data Block = Root
           | Viewport
           | Group
           | Defs
           | Symbol
           deriving (Show)

data Point = Point Float Float deriving (Show)

-- for convenience
type Path = [PathData]

data PathData = Z -- close path 'z'
              | M Point -- moveto 'M'
              | MR Point -- relative moveto 'm'
              | L Point -- lineto
              | LR Point
              | C Point Point Point -- curveto cubic spline, using the first two points as controls and the last point as the destination
              | CR Point Point Point
              | S Point Point -- smooth curveto
              | SR Point Point
              | Q Point Point -- quadtratic spline, using first point as control and second point as the destination
              | QR Point Point
              | T Point -- smooth quadratic spline
              | TR Point
              | A Float Float Float Bool Bool Point -- elliptical arc, with params rx, ry, axis-rotation, large-arc, sweep, destination
              | AR Float Float Float Bool Bool Point
              deriving (Show)

points :: [(Float, Float)] -> [Point]
points = map (uncurry Point)

